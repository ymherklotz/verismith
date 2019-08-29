{-|
Module      : VeriSmith.Reduce
Description : Test case reducer implementation.
Copyright   : (c) 2019, Yann Herklotz
License     : GPL-3
Maintainer  : yann [at] yannherklotz [dot] com
Stability   : experimental
Portability : POSIX

Test case reducer implementation.
-}

{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VeriSmith.Reduce
    ( -- $strategy
      reduceWithScript
    , reduceSynth
    , reduceSynthesis
    , reduce
    , reduce_
    , Replacement(..)
    , halveModules
    , halveModItems
    , halveStatements
    , halveExpr
    , halveAssigns
    , findActiveWires
    , clean
    , cleanSourceInfo
    , cleanSourceInfoAll
    , removeDecl
    , filterExpr
    )
where

import           Control.Lens             hiding ((<.>))
import           Control.Monad            (void)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Foldable            (foldrM)
import           Data.List                (nub)
import           Data.List.NonEmpty       (NonEmpty (..))
import qualified Data.List.NonEmpty       as NonEmpty
import           Data.Maybe               (mapMaybe)
import           Data.Text                (Text)
import           Shelly                   ((<.>))
import qualified Shelly
import           Shelly.Lifted            (MonadSh, liftSh)
import           VeriSmith.Internal
import           VeriSmith.Result
import           VeriSmith.Sim
import           VeriSmith.Sim.Internal
import           VeriSmith.Verilog
import           VeriSmith.Verilog.AST
import           VeriSmith.Verilog.Mutate
import           VeriSmith.Verilog.Parser


-- $strategy
-- The reduction strategy has multiple different steps. 'reduce' will run these
-- strategies one after another, starting at the most coarse grained one. The
-- supported reduction strategies are the following:
--
--   [Modules] First of all, the reducer will try and remove all the modules
--   except the top module.
--
--   [Module Items] Then, the module items will be reduced by using standard
--   delta debugging. Half of the module items will be removed, and both
--   versions will be tested. If both succeed, they will be divided further and
--   tested further. Finally, the shortest version will be returned.
--
--   [Statements] Once the module items have been reduced, the statements will
--   be reduced as well. This is done using delta debugging, just like the
--   module items.
--
--   [Expressions] Finally, the expressions themselves will be reduced. This is
--   done by splitting the top most binary expressions in half and testing each
--   half.

-- | Replacement type that supports returning different kinds of reduced
-- replacements that could be tried.
data Replacement a = Dual a a
                   | Single a
                   | None
                   deriving (Show, Eq)

type Replace a = a -> Replacement a

instance Functor Replacement where
    fmap f (Dual a b) = Dual (f a) $ f b
    fmap f (Single a) = Single $ f a
    fmap _ None       = None

instance Applicative Replacement where
    pure = Single
    (Dual a b) <*> (Dual c d) = Dual (a c) $ b d
    (Dual a b) <*> (Single c) = Dual (a c) $ b c
    (Single a) <*> (Dual b c) = Dual (a b) $ a c
    (Single a) <*> (Single b) = Single $ a b
    None <*> _ = None
    _ <*> None = None

instance Foldable Replacement where
    foldMap _ None       = mempty
    foldMap f (Single a) = f a
    foldMap f (Dual a b) = f a <> f b

instance Traversable Replacement where
    traverse _ None       = pure None
    traverse f (Single a) = Single <$> f a
    traverse f (Dual a b) = Dual <$> f a <*> f b

-- | Split a list in two halves.
halve :: Replace [a]
halve []  = Single []
halve [_] = Single []
halve l   = Dual a b where (a, b) = splitAt (length l `div` 2) l

halveNonEmpty :: Replace (NonEmpty a)
halveNonEmpty l = case NonEmpty.splitAt (length l `div` 2) l of
    ([]   , []   ) -> None
    ([]   , a : b) -> Single $ a :| b
    (a : b, []   ) -> Single $ a :| b
    (a : b, c : d) -> Dual (a :| b) $ c :| d

-- | When given a Lens and a function that works on a lower replacement, it will
-- go down, apply the replacement, and return a replacement of the original
-- module.
combine :: Lens' a b -> Replace b -> Replace a
combine l f i = modify <$> f (i ^. l) where modify res = i & l .~ res

-- | Deletes Id 'Expr' if they are not part of the current scope, and replaces
-- these by 0.
filterExpr :: [Identifier] -> Expr -> Expr
filterExpr ids (Id i) = if i `elem` ids then Id i else Number 0
filterExpr ids (VecSelect i e) =
    if i `elem` ids then VecSelect i e else Number 0
filterExpr ids (RangeSelect i r) =
    if i `elem` ids then RangeSelect i r else Number 0
filterExpr _ e = e

-- | Checks if a declaration is part of the current scope. If not, it returns
-- 'False', otherwise 'True', as it should be kept.
--filterDecl :: [Identifier] -> ModItem -> Bool
--filterDecl ids (Decl Nothing (Port _ _ _ i) _) = i `elem` ids
--filterDecl _   _                               = True

-- | Checks if a continuous assignment is in the current scope, if not, it
-- returns 'False'.
filterAssigns :: [Port] -> ModItem -> Bool
filterAssigns out (ModCA (ContAssign i _)) =
    elem i $ out ^.. traverse . portName
filterAssigns _ _ = True

clean :: (Mutate a) => [Identifier] -> a -> a
clean ids = mutExpr (transform $ filterExpr ids)

cleanUndefined :: [Identifier] -> [ModItem] -> [ModItem]
cleanUndefined ids mis = clean usedWires mis
  where
    usedWires = mis ^.. traverse . modContAssign . contAssignNetLVal <> ids

halveModAssign :: Replace ModDecl
halveModAssign m = cleanMod m $ modify <$> assigns (m ^. modItems)
  where
    assigns = halve . filter (filterAssigns $ m ^. modOutPorts)
    modify l = m & modItems .~ l

cleanMod :: ModDecl -> Replacement ModDecl -> Replacement ModDecl
cleanMod m newm = modify . change <$> newm
  where
    mis = m ^. modItems
    modify l = m & modItems .~ l
    change l =
        cleanUndefined (m ^.. modInPorts . traverse . portName)
            .  combineAssigns (head $ m ^. modOutPorts)
            .  (filter (not . filterAssigns []) mis <>)
            $  l
            ^. modItems

halveIndExpr :: Replace Expr
halveIndExpr (Concat l      ) = Concat <$> halveNonEmpty l
halveIndExpr (BinOp e1 _  e2) = Dual e1 e2
halveIndExpr (Cond  _  e1 e2) = Dual e1 e2
halveIndExpr (UnOp _ e      ) = Single e
halveIndExpr (Appl _ e      ) = Single e
halveIndExpr e                = Single e

halveModExpr :: Replace ModItem
halveModExpr (ModCA ca) = ModCA <$> combine contAssignExpr halveIndExpr ca
halveModExpr a          = Single a

-- | Remove all the undefined mod instances.
cleanModInst :: SourceInfo -> SourceInfo
cleanModInst srcInfo = srcInfo & infoSrc . _Wrapped .~ cleaned
  where
    validInst = srcInfo ^.. infoSrc . _Wrapped . traverse . modId
    cleaned   = cleanModInst' validInst <$> srcInfo ^. infoSrc . _Wrapped

-- | Clean all the undefined module instances in a specific module using a
-- context.
cleanModInst' :: [Identifier] -> ModDecl -> ModDecl
cleanModInst' ids m = m & modItems .~ newModItem
    where newModItem = filter (validModInst ids) $ m ^.. modItems . traverse

-- | Check if a mod instance is in the current context.
validModInst :: [Identifier] -> ModItem -> Bool
validModInst ids (ModInst i _ _) = i `elem` ids
validModInst _   _               = True

-- | Adds a 'ModDecl' to a 'SourceInfo'.
addMod :: ModDecl -> SourceInfo -> SourceInfo
addMod m srcInfo = srcInfo & infoSrc . _Wrapped %~ (m :)

-- | Split a module declaration in half by trying to remove assign
-- statements. This is only done in the main module of the source.
halveAssigns :: Replace SourceInfo
halveAssigns = combine mainModule halveModAssign

-- | Checks if a module item is needed in the module declaration.
relevantModItem :: ModDecl -> ModItem -> Bool
relevantModItem (ModDecl _ out _ _ _) (ModCA (ContAssign i _)) =
    i `elem` fmap _portName out
relevantModItem _ Decl{} = True
relevantModItem _ _      = False

isAssign :: Statement -> Bool
isAssign (BlockAssign    _) = True
isAssign (NonBlockAssign _) = True
isAssign _                  = False

lValName :: LVal -> [Identifier]
lValName (RegId i    ) = [i]
lValName (RegExpr i _) = [i]
lValName (RegSize i _) = [i]
lValName (RegConcat e) = mapMaybe getId . concat $ universe <$> e
  where
    getId (Id i) = Just i
    getId _      = Nothing

-- | Pretending that expr is an LVal for the case that it is in a module
-- instantiation.
exprName :: Expr -> [Identifier]
exprName (Id i           ) = [i]
exprName (VecSelect   i _) = [i]
exprName (RangeSelect i _) = [i]
exprName (Concat i       ) = concat . NonEmpty.toList $ exprName <$> i
exprName _                 = []

-- | Returns the only identifiers that are directly tied to an expression. This
-- is useful if one does not have to recurse deeper into the expressions.
exprId :: Expr -> Maybe Identifier
exprId (Id i           ) = Just i
exprId (VecSelect   i _) = Just i
exprId (RangeSelect i _) = Just i
exprId _                 = Nothing

eventId :: Event -> Maybe Identifier
eventId (EId      i) = Just i
eventId (EPosEdge i) = Just i
eventId (ENegEdge i) = Just i
eventId _            = Nothing

portToId :: Port -> Identifier
portToId (Port _ _ _ i) = i

paramToId :: Parameter -> Identifier
paramToId (Parameter i _) = i

isModule :: Identifier -> ModDecl -> Bool
isModule i (ModDecl n _ _ _ _) = i == n

modInstActive :: [ModDecl] -> ModItem -> [Identifier]
modInstActive decl (ModInst n _ i) = case m of
    Nothing -> []
    Just m' -> concat $ calcActive m' <$> zip i [0 ..]
  where
    m = safe head $ filter (isModule n) decl
    calcActive (ModDecl _ o _ _ _) (ModConn e, n') | n' < length o = exprName e
                                                   | otherwise     = []
    calcActive (ModDecl _ o _ _ _) (ModConnNamed i' e, _)
        | i' `elem` fmap _portName o = exprName e
        | otherwise                  = []
modInstActive _ _ = []

fixModInst :: SourceInfo -> ModItem -> ModItem
fixModInst (SourceInfo _ (Verilog decl)) (ModInst n g i) = case m of
    Nothing -> error "Moditem not found"
    Just m' -> ModInst n g . mapMaybe (fixModInst' m') $ zip i [0 ..]
  where
    m = safe head $ filter (isModule n) decl
    fixModInst' (ModDecl _ o i' _ _) (ModConn e, n')
        | n' < length o + length i' = Just $ ModConn e
        | otherwise                 = Nothing
    fixModInst' (ModDecl _ o i'' _ _) (ModConnNamed i' e, _)
        | i' `elem` fmap _portName (o <> i'') = Just $ ModConnNamed i' e
        | otherwise                           = Nothing
fixModInst _ a = a

findActiveWires :: Identifier -> SourceInfo -> [Identifier]
findActiveWires t src =
    nub
        $  assignWires
        <> assignStat
        <> fmap portToId  i
        <> fmap portToId  o
        <> fmap paramToId p
        <> modinstwires
  where
    assignWires = m ^.. modItems . traverse . modContAssign . contAssignNetLVal
    assignStat =
        concatMap lValName
            $  (allStat ^.. traverse . stmntBA . assignReg)
            <> (allStat ^.. traverse . stmntNBA . assignReg)
    allStat = filter isAssign . concat $ fmap universe stat
    stat =
        (m ^.. modItems . traverse . _Initial)
            <> (m ^.. modItems . traverse . _Always)
    modinstwires =
        concat $ modInstActive (src ^. infoSrc . _Wrapped) <$> m ^. modItems
    m@(ModDecl _ o i _ p) = src ^. aModule t

-- | Clean a specific module. Have to be carful that the module is in the
-- 'SourceInfo', otherwise it will crash.
cleanSourceInfo :: Identifier -> SourceInfo -> SourceInfo
cleanSourceInfo t src = src & aModule t %~ clean (findActiveWires t src)

cleanSourceInfoAll :: SourceInfo -> SourceInfo
cleanSourceInfoAll src = foldr cleanSourceInfo src allMods
    where allMods = src ^.. infoSrc . _Wrapped . traverse . modId

-- | Returns true if the text matches the name of a module.
matchesModName :: Identifier -> ModDecl -> Bool
matchesModName top (ModDecl i _ _ _ _) = top == i

halveStatement :: Replace Statement
halveStatement (SeqBlock [s]) = halveStatement s
halveStatement (SeqBlock s) = SeqBlock <$> halve s
halveStatement (CondStmnt _ (Just s1) (Just s2)) = Dual s1 s2
halveStatement (CondStmnt _ (Just s1) Nothing) = Single s1
halveStatement (CondStmnt _ Nothing (Just s1)) = Single s1
halveStatement (EventCtrl e (Just s)) = EventCtrl e . Just <$> halveStatement s
halveStatement (TimeCtrl e (Just s)) = TimeCtrl e . Just <$> halveStatement s
halveStatement a = Single a

halveAlways :: Replace ModItem
halveAlways (Always s) = Always <$> halveStatement s
halveAlways a          = Single a

-- | Removes half the modules randomly, until it reaches a minimal amount of
-- modules. This is done by doing a binary search on the list of modules and
-- removing the instantiations from the main module body.
halveModules :: Replace SourceInfo
halveModules srcInfo@(SourceInfo top _) =
    cleanSourceInfoAll
        .   cleanModInst
        .   addMod main
        <$> combine (infoSrc . _Wrapped) repl srcInfo
  where
    repl = halve . filter (not . matchesModName (Identifier top))
    main = srcInfo ^. mainModule

moduleBot :: SourceInfo -> Bool
moduleBot (SourceInfo _ (Verilog [] )) = True
moduleBot (SourceInfo _ (Verilog [_])) = True
moduleBot (SourceInfo _ (Verilog _  )) = False

-- | Reducer for module items. It does a binary search on all the module items,
-- except assignments to outputs and input-output declarations.
halveModItems :: Identifier -> Replace SourceInfo
halveModItems t srcInfo = cleanSourceInfo t . addRelevant <$> src
  where
    repl        = halve . filter (not . relevantModItem main)
    relevant    = filter (relevantModItem main) $ main ^. modItems
    main        = srcInfo ^. aModule t
    src         = combine (aModule t . modItems) repl srcInfo
    addRelevant = aModule t . modItems %~ (relevant ++)

modItemBot :: Identifier -> SourceInfo -> Bool
modItemBot t srcInfo | length modItemsNoDecl > 2 = False
                     | otherwise                 = True
  where
    modItemsNoDecl =
        filter noDecl $ srcInfo ^.. aModule t . modItems . traverse
    noDecl Decl{} = False
    noDecl _      = True

halveStatements :: Identifier -> Replace SourceInfo
halveStatements t m =
    cleanSourceInfo t <$> combine (aModule t . modItems) halves m
    where halves = traverse halveAlways

-- | Reduce expressions by splitting them in half and keeping the half that
-- succeeds.
halveExpr :: Identifier -> Replace SourceInfo
halveExpr t = combine contexpr $ traverse halveModExpr
  where
    contexpr :: Lens' SourceInfo [ModItem]
    contexpr = aModule t . modItems

toIds :: [Expr] -> [Identifier]
toIds = nub . mapMaybe exprId . concatMap universe

toIdsConst :: [ConstExpr] -> [Identifier]
toIdsConst = toIds . fmap constToExpr

toIdsEvent :: [Event] -> [Identifier]
toIdsEvent = nub . mapMaybe eventId . concatMap universe

allStatIds' :: Statement -> [Identifier]
allStatIds' s = nub $ assignIds <> otherExpr <> eventProcessedIds
  where
    assignIds =
        toIds
            $  (s ^.. stmntBA . assignExpr)
            <> (s ^.. stmntNBA . assignExpr)
            <> (s ^.. forAssign . assignExpr)
            <> (s ^.. forIncr . assignExpr)
    otherExpr         = toIds $ (s ^.. forExpr) <> (s ^.. stmntCondExpr)
    eventProcessedIds = toIdsEvent $ s ^.. statEvent

allStatIds :: Statement -> [Identifier]
allStatIds s = nub . concat $ allStatIds' <$> universe s

fromRange :: Range -> [ConstExpr]
fromRange r = [rangeMSB r, rangeLSB r]

allExprIds :: ModDecl -> [Identifier]
allExprIds m =
    nub
        $  contAssignIds
        <> modInstIds
        <> modInitialIds
        <> modAlwaysIds
        <> modPortIds
        <> modDeclIds
        <> paramIds
  where
    contAssignIds =
        toIds $ m ^.. modItems . traverse . modContAssign . contAssignExpr
    modInstIds =
        toIds $ m ^.. modItems . traverse . modInstConns . traverse . modExpr
    modInitialIds =
        nub . concatMap allStatIds $ m ^.. modItems . traverse . _Initial
    modAlwaysIds =
        nub . concatMap allStatIds $ m ^.. modItems . traverse . _Always
    modPortIds =
        nub
            .   concatMap (toIdsConst . fromRange)
            $   m
            ^.. modItems
            .   traverse
            .   declPort
            .   portSize
    modDeclIds = toIdsConst $ m ^.. modItems . traverse . declVal . _Just
    paramIds =
        toIdsConst
            $  (m ^.. modItems . traverse . paramDecl . traverse . paramValue)
            <> (   m
               ^.. modItems
               .   traverse
               .   localParamDecl
               .   traverse
               .   localParamValue
               )

isUsedDecl :: [Identifier] -> ModItem -> Bool
isUsedDecl ids (Decl _ (Port _ _ _ i) _) = i `elem` ids
isUsedDecl _   _                         = True

isUsedParam :: [Identifier] -> Parameter -> Bool
isUsedParam ids (Parameter i _) = i `elem` ids

isUsedPort :: [Identifier] -> Port -> Bool
isUsedPort ids (Port _ _ _ i) = i `elem` ids

removeDecl :: SourceInfo -> SourceInfo
removeDecl src = foldr fix removed allMods
  where
    removeDecl' t src' =
        src'
            & (\a -> a & aModule t . modItems %~ filter
                  (isUsedDecl (used <> findActiveWires t a))
              )
            . (aModule t . modParams %~ filter (isUsedParam used))
            . (aModule t . modInPorts %~ filter (isUsedPort used))
        where used = nub $ allExprIds (src' ^. aModule t)
    allMods = src ^.. infoSrc . _Wrapped . traverse . modId
    fix t a = a & aModule t . modItems %~ fmap (fixModInst a)
    removed = foldr removeDecl' src allMods

defaultBot :: SourceInfo -> Bool
defaultBot = const False

-- | Reduction using custom reduction strategies.
reduce_
    :: MonadSh m
    => Text
    -> Replace SourceInfo
    -> (SourceInfo -> Bool)
    -> (SourceInfo -> m Bool)
    -> SourceInfo
    -> m SourceInfo
reduce_ title repl bot eval src = do
    liftSh
        .  Shelly.echo
        $  "Reducing "
        <> title
        <> " (Modules: "
        <> showT (length . getVerilog $ _infoSrc src)
        <> ", Module items: "
        <> showT
               (length
                   (src ^.. infoSrc . _Wrapped . traverse . modItems . traverse)
               )
        <> ")"
    if bot src
        then return src
        else case repl src of
            Single s -> do
                red <- eval s
                if red
                    then if cond s then recReduction s else return s
                    else return src
            Dual l r -> do
                red <- eval l
                if red
                    then if cond l then recReduction l else return l
                    else do
                        red' <- eval r
                        if red'
                            then if cond r then recReduction r else return r
                            else return src
            None -> return src
  where
    cond s = s /= src
    recReduction = reduce_ title repl bot eval

-- | Reduce an input to a minimal representation. It follows the reduction
-- strategy mentioned above.
reduce
    :: MonadSh m
    => (SourceInfo -> m Bool) -- ^ Failed or not.
    -> SourceInfo              -- ^ Input verilog source to be reduced.
    -> m SourceInfo           -- ^ Reduced output.
reduce eval src =
    fmap removeDecl
        $   red "Modules" moduleBot halveModules src
        >>= redAll "Module Items" modItemBot         halveModItems
        >>= redAll "Statements"   (const defaultBot) halveStatements
        -- >>= redAll "Expressions"  (const defaultBot) halveExpr
  where
    red s bot a = reduce_ s a bot eval
    red' s bot a t = reduce_ s (a t) (bot t) eval
    redAll s bot halve' src' = foldrM
        (\t -> red' (s <> " (" <> getIdentifier t <> ")") bot halve' t)
        src'
        (src' ^.. infoSrc . _Wrapped . traverse . modId)

runScript
    :: MonadSh m => Shelly.FilePath -> Shelly.FilePath -> SourceInfo -> m Bool
runScript fp file src = do
    e <- liftSh $ do
        Shelly.writefile file $ genSource src
        noPrint . Shelly.errExit False $ Shelly.run_ fp []
        Shelly.lastExitCode
    return $ e == 0

-- | Reduce using a script that is passed to it
reduceWithScript
    :: (MonadSh m, MonadIO m)
    => Text
    -> Shelly.FilePath
    -> Shelly.FilePath
    -> m ()
reduceWithScript top script file = do
    liftSh . Shelly.cp file $ file <.> "original"
    srcInfo <- liftIO . parseSourceInfoFile top $ Shelly.toTextIgnore file
    void $ reduce (runScript script file) srcInfo

-- | Reduce a 'SourceInfo' using two 'Synthesiser' that are passed to it.
reduceSynth
    :: (Synthesiser a, Synthesiser b, MonadSh m)
    => a
    -> b
    -> SourceInfo
    -> m SourceInfo
reduceSynth a b = reduce synth
  where
    synth src' = liftSh $ do
        r <- runResultT $ do
            runSynth a src'
            runSynth b src'
            runEquiv a b src'
        return $ case r of
            Fail EquivFail -> True
            Fail _         -> False
            Pass _         -> False

reduceSynthesis :: (Synthesiser a, MonadSh m) => a -> SourceInfo -> m SourceInfo
reduceSynthesis a = reduce synth
  where
    synth src = liftSh $ do
        r <- runResultT $ runSynth a src
        return $ case r of
            Fail SynthFail -> True
            Fail _         -> False
            Pass _         -> False
