{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Verismith.Reduce
-- Description : Test case reducer implementation.
-- Copyright   : (c) 2019, Yann Herklotz
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- Test case reducer implementation.
module Verismith.Reduce
  ( -- $strategy
    reduceWithScript,
    reduceSynth,
    reduceSynthesis,
    reduceSimIc,
    reduce,
    reduce_,
    Replacement (..),
    halveModules,
    halveModItems,
    halveStatements,
    halveExpr,
    halveAssigns,
    findActiveWires,
    clean,
    cleanSourceInfo,
    cleanSourceInfoAll,
    removeDecl,
    removeConstInConcat,
    takeReplace,
    filterExpr,
    ReduceAnn (..),
    tagAlways,
    untagAlways,
  )
where

import Control.Lens hiding ((<.>))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.Foldable (foldrM)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Text (Text, unpack)
import Shelly ((<.>), fromText)
import qualified Shelly
import Shelly.Lifted (MonadSh, liftSh, rm_rf, writefile)
import Verismith.Internal
import Verismith.Result
import Verismith.Tool
import Verismith.Tool.Icarus
import Verismith.Tool.Identity
import Verismith.Tool.Internal
import Verismith.Verilog
import Verismith.Verilog.AST
import Verismith.Verilog.CodeGen
import Verismith.Verilog.Mutate
import Verismith.Verilog.Parser

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
data Replacement a
  = Dual a a
  | Single a
  | None
  deriving (Show, Eq)

data ReduceAnn
  = Active
  | Reduced
  | Idle
  deriving (Show, Eq)

type Replace a = (a -> Replacement a)

instance Functor Replacement where
  fmap f (Dual a b) = Dual (f a) $ f b
  fmap f (Single a) = Single $ f a
  fmap _ None = None

instance Applicative Replacement where
  pure = Single
  (Dual a b) <*> (Dual c d) = Dual (a c) $ b d
  (Dual a b) <*> (Single c) = Dual (a c) $ b c
  (Single a) <*> (Dual b c) = Dual (a b) $ a c
  (Single a) <*> (Single b) = Single $ a b
  None <*> _ = None
  _ <*> None = None

instance Foldable Replacement where
  foldMap _ None = mempty
  foldMap f (Single a) = f a
  foldMap f (Dual a b) = f a <> f b

instance Traversable Replacement where
  traverse _ None = pure None
  traverse f (Single a) = Single <$> f a
  traverse f (Dual a b) = Dual <$> f a <*> f b

-- | Split a list in two halves.
halve :: Replace [a]
halve [] = Single []
halve [_] = Single []
halve l = Dual a b where (a, b) = splitAt (length l `div` 2) l

remove1 :: Replace [a]
remove1 [] = Single []
remove1 [_] = Single []
remove1 (a : b) = Dual [a] b

halveNonEmpty :: Replace (NonEmpty a)
halveNonEmpty l = case NonEmpty.splitAt (length l `div` 2) l of
  ([], []) -> None
  ([], a : b) -> Single $ a :| b
  (a : b, []) -> Single $ a :| b
  (a : b, c : d) -> Dual (a :| b) $ c :| d

-- | When given a Lens and a function that works on a lower replacement, it will
-- go down, apply the replacement, and return a replacement of the original
-- module.
combine :: (Monoid b) => Traversal' a b -> Replace b -> Replace a
combine l f i = modify <$> f (i ^. l) where modify res = i & l .~ res

-- | When given a Lens and a function that works on a lower replacement, it will
-- go down, apply the replacement, and return a replacement of the original
-- module.
combineL :: Lens' a b -> Replace b -> Replace a
combineL l f i = modify <$> f (i ^. l) where modify res = i & l .~ res

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
-- filterDecl :: [Identifier] -> (ModItem ReduceAnn) -> Bool
-- filterDecl ids (Decl Nothing (Port _ _ _ i) _) = i `elem` ids
-- filterDecl _   _                               = True

-- | Checks if a continuous assignment is in the current scope, if not, it
-- returns 'False'.
filterAssigns :: [Port] -> (ModItem ReduceAnn) -> Bool
filterAssigns out (ModCA (ContAssign i _)) =
  elem i $ out ^.. traverse . portName
filterAssigns _ _ = True

clean :: (Mutate a) => [Identifier] -> a -> a
clean ids = mutExpr (transform $ filterExpr ids)

takeReplace :: (Monoid a) => Replacement a -> a
takeReplace (Single a) = a
takeReplace (Dual a _) = a
takeReplace None = mempty

-- | Remove all the constants that are in the concatination.
removeConstInConcat :: Replace (SourceInfo ReduceAnn)
removeConstInConcat = Single . mutExpr replace
  where
    replace :: Expr -> Expr
    replace (Concat expr) =
      maybe (Number 0) Concat . NonEmpty.nonEmpty $
        NonEmpty.filter notConstant expr
    replace e = e
    notConstant (Number _) = False
    notConstant _ = True

cleanUndefined :: [Identifier] -> [ModItem ReduceAnn] -> [ModItem ReduceAnn]
cleanUndefined ids mis = clean usedWires mis
  where
    usedWires = mis ^.. traverse . modContAssign . contAssignNetLVal <> ids

halveModAssign :: Replace (ModDecl ReduceAnn)
halveModAssign m = cleanMod m $ modify <$> assigns (m ^. modItems)
  where
    assigns = halve . filter (filterAssigns $ m ^. modOutPorts)
    modify l = m & modItems .~ l

cleanMod :: (ModDecl ReduceAnn) -> Replacement (ModDecl ReduceAnn) -> Replacement (ModDecl ReduceAnn)
cleanMod m newm = modify . change <$> newm
  where
    mis = m ^. modItems
    modify l = m & modItems .~ l
    change l =
      cleanUndefined (m ^.. modInPorts . traverse . portName)
        . combineAssigns (head $ m ^. modOutPorts)
        . (filter (not . filterAssigns []) mis <>)
        $ l
          ^. modItems

halveIndExpr :: Replace Expr
halveIndExpr (Concat l) = Concat <$> halveNonEmpty l
halveIndExpr (BinOp e1 _ e2) = Dual e1 e2
halveIndExpr (Cond _ e1 e2) = Dual e1 e2
halveIndExpr (UnOp _ e) = Single e
halveIndExpr (Appl _ e) = Single e
halveIndExpr e = Single e

halveModExpr :: Replace (ModItem ReduceAnn)
halveModExpr (ModCA ca) = ModCA <$> combine contAssignExpr halveIndExpr ca
halveModExpr a = Single a

-- | Split a module declaration in half by trying to remove assign
-- statements. This is only done in the main module of the source.
halveAssigns :: Replace (SourceInfo ReduceAnn)
halveAssigns = combineL mainModule halveModAssign

-- | Checks if a module item is needed in the module declaration.
relevantModItem :: (ModDecl ReduceAnn) -> (ModItem ReduceAnn) -> Bool
relevantModItem (ModDecl _ out _ _ _) (ModCA (ContAssign i _)) =
  i `elem` fmap _portName out
relevantModItem _ Decl {} = True
relevantModItem _ _ = False

isAssign :: (Statement ReduceAnn) -> Bool
isAssign (BlockAssign _) = True
isAssign (NonBlockAssign _) = True
isAssign (ForLoop _ _ _ _) = True
isAssign _ = False

lValName :: LVal -> [Identifier]
lValName (RegId i) = [i]
lValName (RegExpr i _) = [i]
lValName (RegSize i _) = [i]
lValName (RegConcat e) = mapMaybe getId . concat $ universe <$> e
  where
    getId (Id i) = Just i
    getId _ = Nothing

-- | Pretending that expr is an LVal for the case that it is in a module
-- instantiation.
exprName :: Expr -> [Identifier]
exprName (Id i) = [i]
exprName (VecSelect i _) = [i]
exprName (RangeSelect i _) = [i]
exprName (Concat i) = concat . NonEmpty.toList $ exprName <$> i
exprName _ = []

-- | Returns the only identifiers that are directly tied to an expression. This
-- is useful if one does not have to recurse deeper into the expressions.
exprId :: Expr -> Maybe Identifier
exprId (Id i) = Just i
exprId (VecSelect i _) = Just i
exprId (RangeSelect i _) = Just i
exprId _ = Nothing

eventId :: Event -> Maybe Identifier
eventId (EId i) = Just i
eventId (EPosEdge i) = Just i
eventId (ENegEdge i) = Just i
eventId _ = Nothing

portToId :: Port -> Identifier
portToId (Port _ _ _ i) = i

paramToId :: Parameter -> Identifier
paramToId (Parameter i _) = i

isModule :: Identifier -> (ModDecl ReduceAnn) -> Bool
isModule i (ModDecl n _ _ _ _) = i == n

modInstActive :: [(ModDecl ReduceAnn)] -> (ModItem ReduceAnn) -> [Identifier]
modInstActive decl (ModInst n _ i) = case m of
  Nothing -> []
  Just m' -> concat $ calcActive m' <$> zip i [0 ..]
  where
    m = safe head $ filter (isModule n) decl
    calcActive (ModDecl _ o _ _ _) (ModConn e, n')
      | n' < length o = exprName e
      | otherwise = []
    calcActive (ModDecl _ o _ _ _) (ModConnNamed i' e, _)
      | i' `elem` fmap _portName o = exprName e
      | otherwise = []
modInstActive _ _ = []

fixModInst :: (SourceInfo ReduceAnn) -> (ModItem ReduceAnn) -> (ModItem ReduceAnn)
fixModInst (SourceInfo _ (Verilog decl)) (ModInst n g i) = case m of
  Nothing -> error "Moditem not found"
  Just m' -> ModInst n g . mapMaybe (fixModInst' m') $ zip i [0 ..]
  where
    m = safe head $ filter (isModule n) decl
    fixModInst' (ModDecl _ o i' _ _) (ModConn e, n')
      | n' < length o + length i' = Just $ ModConn e
      | otherwise = Nothing
    fixModInst' (ModDecl _ o i'' _ _) (ModConnNamed i' e, _)
      | i' `elem` fmap _portName (o <> i'') = Just $ ModConnNamed i' e
      | otherwise = Nothing
fixModInst _ a = a

eventIdent :: Event -> [Identifier]
eventIdent (EId i) = [i]
eventIdent (EExpr e) =
  case exprId e of
    Nothing -> []
    Just eid -> [eid]
eventIdent EAll = []
eventIdent (EPosEdge i) = [i]
eventIdent (ENegEdge i) = [i]
eventIdent (EOr e1 e2) = eventIdent e1 <> eventIdent e2
eventIdent (EComb e1 e2) = eventIdent e1 <> eventIdent e2

findActiveWires :: Identifier -> (SourceInfo ReduceAnn) -> [Identifier]
findActiveWires t src =
  nub $
    assignWires
      <> assignStat
      <> fmap portToId i
      <> fmap portToId o
      <> fmap paramToId p
      <> modinstwires
      <> events
  where
    assignWires = m ^.. modItems . traverse . modContAssign . contAssignNetLVal
    assignStat =
      concatMap lValName $
        (allStat ^.. traverse . stmntBA . assignReg)
          <> (allStat ^.. traverse . stmntNBA . assignReg)
          <> (allStat ^.. traverse . forAssign . assignReg)
          <> (allStat ^.. traverse . forIncr . assignReg)
    events = concatMap eventIdent $ (allStat ^.. traverse . statEvent)
    allStat = filter isAssign . concat $ fmap universe stat
    stat =
      (m ^.. modItems . traverse . _Initial)
        <> (m ^.. modItems . traverse . _Always)
    modinstwires =
      concat $ modInstActive (src ^. infoSrc . _Wrapped) <$> m ^. modItems
    m@(ModDecl _ o i _ p) = src ^. aModule t

-- | Clean a specific module. Have to be carful that the module is in the
-- '(SourceInfo ReduceAnn)', otherwise it will crash.
cleanSourceInfo :: Identifier -> (SourceInfo ReduceAnn) -> (SourceInfo ReduceAnn)
cleanSourceInfo t src = src & aModule t %~ clean (findActiveWires t src)

cleanSourceInfoAll :: (SourceInfo ReduceAnn) -> (SourceInfo ReduceAnn)
cleanSourceInfoAll src = foldr cleanSourceInfo src allMods
  where
    allMods = src ^.. infoSrc . _Wrapped . traverse . modId

-- | Returns true if the text matches the name of a module.
matchesModName :: Identifier -> (ModDecl ReduceAnn) -> Bool
matchesModName top (ModDecl i _ _ _ _) = top == i

halveStatement :: Replace (Statement ReduceAnn)
halveStatement (SeqBlock [s]) = halveStatement s
halveStatement (SeqBlock s) = SeqBlock <$> halve s
halveStatement (CondStmnt _ (Just s1) (Just s2)) = Dual s1 s2
halveStatement (CondStmnt _ (Just s1) Nothing) = Single s1
halveStatement (CondStmnt _ Nothing (Just s1)) = Single s1
halveStatement (EventCtrl e (Just s)) = EventCtrl e . Just <$> halveStatement s
halveStatement (TimeCtrl e (Just s)) = TimeCtrl e . Just <$> halveStatement s
halveStatement a = Single a

halveAlways :: Replace (ModItem ReduceAnn)
halveAlways (ModItemAnn Active (Always s)) = ModItemAnn Active . Always <$> halveStatement s
halveAlways r@(ModItemAnn Reduced (Always s)) = Single r
halveAlways a = Single a

-- | Check if a mod instance is in the current context.
validModInst :: [Identifier] -> (ModItem ReduceAnn) -> Bool
validModInst ids (ModInst i _ _) = i `elem` ids
validModInst _ _ = True

-- | Clean all the undefined module instances in a specific module using a
-- context.
cleanModInst' :: [Identifier] -> (ModDecl ReduceAnn) -> (ModDecl ReduceAnn)
cleanModInst' ids m = m & modItems .~ newModItem
  where
    newModItem = filter (validModInst ids) $ m ^.. modItems . traverse

-- | Remove all the undefined mod instances.
cleanModInst :: (SourceInfo ReduceAnn) -> (SourceInfo ReduceAnn)
cleanModInst srcInfo = srcInfo & infoSrc . _Wrapped .~ cleaned
  where
    validInst = srcInfo ^.. infoSrc . _Wrapped . traverse . modId
    cleaned = cleanModInst' validInst <$> srcInfo ^. infoSrc . _Wrapped

-- | Adds a '(ModDecl ReduceAnn)' to a '(SourceInfo ReduceAnn)'.
addMod :: (ModDecl ReduceAnn) -> (SourceInfo ReduceAnn) -> (SourceInfo ReduceAnn)
addMod m srcInfo = srcInfo & infoSrc . _Wrapped %~ (m :)

-- | Removes half the modules randomly, until it reaches a minimal amount of
-- modules. This is done by doing a binary search on the list of modules and
-- removing the instantiations from the main module body.
halveModules :: Replace (SourceInfo ReduceAnn)
halveModules srcInfo@(SourceInfo top _) =
  cleanSourceInfoAll
    . cleanModInst
    . addMod main
    <$> combine (infoSrc . _Wrapped) repl srcInfo
  where
    repl = halve . filter (not . matchesModName (Identifier top))
    main = srcInfo ^. mainModule

moduleBot :: (SourceInfo ReduceAnn) -> Bool
moduleBot (SourceInfo _ (Verilog [])) = True
moduleBot (SourceInfo _ (Verilog [_])) = True
moduleBot (SourceInfo _ (Verilog _)) = False

-- | Reducer for module items. It does a binary search on all the module items,
-- except assignments to outputs and input-output declarations.
halveModItems :: Identifier -> Replace (SourceInfo ReduceAnn)
halveModItems t srcInfo = cleanSourceInfo t . addRelevant <$> src
  where
    repl = halve . filter (not . relevantModItem main)
    relevant = filter (relevantModItem main) $ main ^. modItems
    main = srcInfo ^. aModule t
    src = combine (aModule t . modItems) repl srcInfo
    addRelevant = aModule t . modItems %~ (relevant ++)

modItemBot :: Identifier -> (SourceInfo ReduceAnn) -> Bool
modItemBot t srcInfo
  | length modItemsNoDecl > 2 = False
  | otherwise = True
  where
    modItemsNoDecl =
      filter noDecl $ srcInfo ^.. aModule t . modItems . traverse
    noDecl Decl {} = False
    noDecl _ = True

halveStatements :: Identifier -> Replace (SourceInfo ReduceAnn)
halveStatements t m =
  cleanSourceInfo t <$> combine (aModule t . modItems) (traverse halveAlways) m

-- | Reduce expressions by splitting them in half and keeping the half that
-- succeeds.
halveExpr :: Identifier -> Replace (SourceInfo ReduceAnn)
halveExpr t = combine (aModule t . modItems) $ traverse halveModExpr

toIds :: [Expr] -> [Identifier]
toIds = nub . mapMaybe exprId . concatMap universe

toIdsConst :: [ConstExpr] -> [Identifier]
toIdsConst = toIds . fmap constToExpr

toIdsEvent :: [Event] -> [Identifier]
toIdsEvent = nub . mapMaybe eventId . concatMap universe

allStatIds' :: (Statement ReduceAnn) -> [Identifier]
allStatIds' s = nub $ assignIds <> otherExpr <> eventProcessedIds
  where
    assignIds =
      toIds $
        (s ^.. stmntBA . assignExpr)
          <> (s ^.. stmntNBA . assignExpr)
          <> (s ^.. forAssign . assignExpr)
          <> (s ^.. forIncr . assignExpr)
    otherExpr = toIds $ (s ^.. forExpr) <> (s ^.. stmntCondExpr)
    eventProcessedIds = toIdsEvent $ s ^.. statEvent

allStatIds :: (Statement ReduceAnn) -> [Identifier]
allStatIds s = nub . concat $ allStatIds' <$> universe s

fromRange :: Range -> [ConstExpr]
fromRange r = [rangeMSB r, rangeLSB r]

allExprIds :: (ModDecl ReduceAnn) -> [Identifier]
allExprIds m =
  nub $
    contAssignIds
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
        . concatMap (toIdsConst . fromRange)
        $ m
          ^.. modItems
          . traverse
          . declPort
          . portSize
    modDeclIds = toIdsConst $ m ^.. modItems . traverse . declVal . _Just
    paramIds =
      toIdsConst $
        (m ^.. modItems . traverse . paramDecl . traverse . paramValue)
          <> ( m
                 ^.. modItems
                 . traverse
                 . localParamDecl
                 . traverse
                 . localParamValue
             )

isUsedDecl :: [Identifier] -> (ModItem ReduceAnn) -> Bool
isUsedDecl ids (Decl _ (Port _ _ _ i) _) = i `elem` ids
isUsedDecl _ _ = True

isUsedParam :: [Identifier] -> Parameter -> Bool
isUsedParam ids (Parameter i _) = i `elem` ids

isUsedPort :: [Identifier] -> Port -> Bool
isUsedPort ids (Port _ _ _ i) = i `elem` ids

-- | Should return true if there is any active tag present.
checkActiveTag :: ModDecl ReduceAnn -> Bool
checkActiveTag m = (/= []) . filter hasActiveTag $ _modItems m
  where
    hasActiveTag (ModItemAnn Active (Always _)) = True
    hasActiveTag _ = False

tagAlwaysBlockMis :: [ModItem ReduceAnn] -> [ModItem ReduceAnn]
tagAlwaysBlockMis [] = []
tagAlwaysBlockMis (mi@(Always _) : mis) = ModItemAnn Active mi : mis
tagAlwaysBlockMis (mi : mis) = mi : tagAlwaysBlockMis mis

-- | Tag an always block to be reduced if there are no active ones.
tagAlwaysBlock :: ModDecl ReduceAnn -> ModDecl ReduceAnn
tagAlwaysBlock m
  | checkActiveTag m = m
  | otherwise = m {_modItems = tagAlwaysBlockMis (_modItems m)}

tagAlwaysBlockReducedMis :: [ModItem ReduceAnn] -> [ModItem ReduceAnn]
tagAlwaysBlockReducedMis [] = []
tagAlwaysBlockReducedMis ((ModItemAnn Active mi) : mis) =
  ModItemAnn Reduced mi : tagAlwaysBlockReducedMis mis
tagAlwaysBlockReducedMis (mi : mis) = mi : tagAlwaysBlockReducedMis mis

-- | Tag an always block to be reduced if there are no active ones.
tagAlwaysBlockReduced :: ModDecl ReduceAnn -> ModDecl ReduceAnn
tagAlwaysBlockReduced m = m {_modItems = tagAlwaysBlockReducedMis (_modItems m)}

tAlways ::
  (ModDecl ReduceAnn -> ModDecl ReduceAnn) ->
  Identifier ->
  SourceInfo ReduceAnn ->
  SourceInfo ReduceAnn
tAlways f t m =
  m & aModule t %~ f

tagAlways, untagAlways, idTag :: Identifier -> SourceInfo ReduceAnn -> SourceInfo ReduceAnn
tagAlways = tAlways tagAlwaysBlock
untagAlways = tAlways tagAlwaysBlockReduced
idTag = const id

removeDecl :: SourceInfo ReduceAnn -> SourceInfo ReduceAnn
removeDecl src = foldr fix removed allMods
  where
    removeDecl' t src' =
      src'
        & ( \a ->
              a & aModule t . modItems
                %~ filter
                  (isUsedDecl (used <> findActiveWires t a))
          )
        . (aModule t . modParams %~ filter (isUsedParam used))
        . (aModule t . modInPorts %~ filter (isUsedPort used))
      where
        used = nub $ allExprIds (src' ^. aModule t)
    allMods = src ^.. infoSrc . _Wrapped . traverse . modId
    fix t a = a & aModule t . modItems %~ fmap (fixModInst a)
    removed = foldr removeDecl' src allMods

defaultBot :: (SourceInfo ReduceAnn) -> Bool
defaultBot = const False

-- | Reduction using custom reduction strategies.
reduce_ ::
  (MonadSh m) =>
  Shelly.FilePath ->
  (SourceInfo ReduceAnn -> m Bool) ->
  Text ->
  (SourceInfo ReduceAnn -> SourceInfo ReduceAnn) ->
  (SourceInfo ReduceAnn -> SourceInfo ReduceAnn) ->
  Replace (SourceInfo ReduceAnn) ->
  (SourceInfo ReduceAnn -> Bool) ->
  SourceInfo ReduceAnn ->
  m (SourceInfo ReduceAnn)
reduce_ out eval title tag untag repl bot usrc = do
  writefile out $ genSource src
  liftSh
    . Shelly.echo
    $ "Reducing " <> title <> " (modules: "
      <> showT (length . getVerilog $ _infoSrc src)
      <> ", module items: "
      <> showT (length (src ^.. infoSrc . _Wrapped . traverse . modItems . traverse))
      <> ", loc: "
      <> showT (length . lines . unpack $ genSource usrc)
      <> ")"
  if bot src
    then return $ untag src
    else case repl src of
      Single s -> do
        red <- eval s
        if red
          then if s /= src then recReduction s else return $ untag src
          else return $ untag src
      Dual l r -> do
        red <- eval l
        if red
          then if l /= src then recReduction l else return $ untag src
          else do
            red' <- eval r
            if red'
              then if r /= src then recReduction r else return $ untag src
              else return $ untag src
      None -> return $ untag src
  where
    src = tag usrc
    recReduction = reduce_ out eval title tag untag repl bot

-- | Reduce an input to a minimal representation. It follows the reduction
-- strategy mentioned above.
reduce ::
  (MonadSh m) =>
  -- | Filepath for temporary file.
  Shelly.FilePath ->
  -- | Failed or not.
  (SourceInfo ReduceAnn -> m Bool) ->
  -- | Input verilog source to be reduced.
  SourceInfo () ->
  -- | Reduced output.
  m (SourceInfo ())
reduce fp eval rsrc =
  fmap (clearAnn . removeDecl) $
    red "Modules" id id halveModules moduleBot src
      >>= redAll "Module items" idTag idTag halveModItems modItemBot
      >>= redAll "Statements" tagAlways untagAlways halveStatements (const defaultBot)
      -- >>= redAll "Expressions" halveExpr (const defaultBot)
      >>= red "Remove constants in concat" id id removeConstInConcat defaultBot
      >>= red "Cleaning" id id (pure . removeDecl) defaultBot
  where
    red = reduce_ fp eval
    redAll s tag untag halve' bot src' =
      foldrM
        (\t -> red (s <> " (" <> getIdentifier t <> ")") (tag t) (untag t) (halve' t) (bot t))
        src'
        (src' ^.. infoSrc . _Wrapped . traverse . modId)
    src = fmap (\_ -> Idle) rsrc

runScript ::
  (MonadSh m, Show ann) =>
  Shelly.FilePath ->
  Shelly.FilePath ->
  (SourceInfo ann) ->
  m Bool
runScript fp file src = do
  e <- liftSh $ do
    Shelly.writefile file $ genSource src
    noPrint . Shelly.errExit False $ Shelly.run_ fp []
    Shelly.lastExitCode
  return $ e == 0

-- | Reduce using a script that is passed to it
reduceWithScript ::
  (MonadSh m, MonadIO m) =>
  Text ->
  Shelly.FilePath ->
  Shelly.FilePath ->
  m ()
reduceWithScript top script file = do
  liftSh . Shelly.cp file $ file <.> "original"
  (srcInfo :: SourceInfo ()) <- liftIO . parseSourceInfoFile top $ Shelly.toTextIgnore file
  void $ reduce (fromText "reduce_script.v") (runScript script file) srcInfo

-- | Reduce a '(SourceInfo ReduceAnn)' using two 'Synthesiser' that are passed to it.
reduceSynth ::
  (Synthesiser a, Synthesiser b, MonadSh m) =>
  Maybe Text ->
  Shelly.FilePath ->
  a ->
  b ->
  SourceInfo () ->
  m (SourceInfo ())
reduceSynth mt datadir a b src = do
  counter <- liftSh . liftIO $ newIORef (0 :: Int)
  reduce (fromText $ prefix <> ".v") (synth counter) src
  where
    synth counter src' = liftSh $ do
      count <- liftIO $ readIORef counter
      liftIO $ writeIORef counter (count + 1)
      Shelly.mkdir (fromText $ prefix <> "_" <> showT count)
      current_dir <- Shelly.pwd
      Shelly.cd (fromText $ prefix <> "_" <> showT count)
      r <- runResultT $ do
        runSynth a src'
        runSynth b src'
        runEquiv mt datadir a b src'
      Shelly.cd current_dir
      return $ case r of
        Fail (EquivFail _) -> True
        _ -> False
    prefix = "reduce_" <> toText a <> "_" <> toText b

reduceSynthesis :: (Synthesiser a, MonadSh m) => a -> SourceInfo () -> m (SourceInfo ())
reduceSynthesis a = reduce (fromText $ "reduce_" <> toText a <> ".v") synth
  where
    synth src = liftSh $ do
      r <- runResultT $ runSynth a src
      return $ case r of
        Fail SynthFail -> True
        _ -> False

runInTmp :: Shelly.Sh a -> Shelly.Sh a
runInTmp a =
  Shelly.withTmpDir $
    ( \f -> do
        dir <- Shelly.pwd
        Shelly.cd f
        r <- a
        Shelly.cd dir
        return r
    )

reduceSimIc ::
  (Synthesiser a, MonadSh m) =>
  Shelly.FilePath ->
  [ByteString] ->
  a ->
  SourceInfo () ->
  m (SourceInfo ())
reduceSimIc fp bs a = reduce (fromText $ "reduce_sim_" <> toText a <> ".v") synth
  where
    synth src = liftSh . runInTmp $ do
      r <- runResultT $ do
        runSynth a src
        runSynth defaultIdentity src
        i <- runSimIc fp defaultIcarus defaultIdentity src bs Nothing
        runSimIc fp defaultIcarus a src bs $ Just i
      return $ case r of
        Fail (SimFail _) -> True
        _ -> False
