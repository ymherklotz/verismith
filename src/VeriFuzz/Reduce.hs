{-|
Module      : VeriFuzz.Reduce
Description : Test case reducer implementation.
Copyright   : (c) 2019, Yann Herklotz
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Test case reducer implementation.
-}

{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VeriFuzz.Reduce
    ( -- $strategy
      Replacement(..)
    , reduce
    , reduce_
    , halveModules
    , halveModItems
    , halveExpr
    , findActiveWires
    )
where

import           Control.Lens
import           Data.List                (nub)
import           Data.Maybe               (mapMaybe)
import           Data.Text                (Text)
import           VeriFuzz.Verilog.AST
import           VeriFuzz.Verilog.CodeGen
import           VeriFuzz.Verilog.Mutate

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
halve []  = None
halve [_] = Single []
halve l   = Dual a b where (a, b) = splitAt (length l `div` 2) l

-- | When given a Lens and a function that works on a lower replacement, it will
-- go down, apply the replacement, and return a replacement of the original
-- module.
combine :: Lens' a b -> Replace b -> Replace a
combine l f i = modify <$> f (i ^. l) where modify res = i & l .~ res

-- | Deletes Id 'Expr' if they are not part of the current scope, and replaces
-- these by 0.
filterExpr :: [Identifier] -> Expr -> Expr
filterExpr ids (Id i) = if i `elem` ids then Id i else Number 0
filterExpr _   e      = e

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
clean ids a = mutExpr (transform $ filterExpr ids) a

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

-- halveStatements :: Statement -> Replacement Statement
-- halveStatements (SeqBlock l            ) = SeqBlock <$> halve l
-- halveStatements (CondStmnt _ (Just a) b) = maybe (Single a) (Dual a) b
-- halveStatements (CondStmnt _ Nothing  b) = maybe None Single b
-- halveStatements (ForLoop _ _ _ s       ) = Single s
-- halveStatements _                        = None

halveIndExpr :: Replace Expr
halveIndExpr (Concat l      ) = Concat <$> halve l
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
        cleaned = cleanModInst' validInst <$> srcInfo ^. infoSrc . _Wrapped

-- | Clean all the undefined module instances in a specific module using a
-- context.
cleanModInst' :: [Identifier] -> ModDecl -> ModDecl
cleanModInst' ids m = m & modItems .~ newModItem
    where
        newModItem = filter (validModInst ids) $ m ^.. modItems . traverse

-- | Check if a mod instance is in the current context.
validModInst :: [Identifier] -> ModItem -> Bool
validModInst ids (ModInst i _ _) = i `elem` ids
validModInst _ _                 = True

-- | Adds a 'ModDecl' to a 'SourceInfo'.
addMod :: ModDecl -> SourceInfo -> SourceInfo
addMod m srcInfo = srcInfo & infoSrc . _Wrapped %~ (m :)

-- | Returns true if the text matches the name of a module.
matchesModName :: Text -> ModDecl -> Bool
matchesModName top (ModDecl i _ _ _ _) = top == getIdentifier i

-- | Removes half the modules randomly, until it reaches a minimal amount of
-- modules. This is done by doing a binary search on the list of modules and
-- removing the instantiations from the main module body.
halveModules :: Replace SourceInfo
halveModules srcInfo@(SourceInfo top _) =
    cleanModInst . addMod main <$> combine (infoSrc . _Wrapped) repl srcInfo
    where
        repl = halve . filter (not . matchesModName top)
        main = srcInfo ^. mainModule

-- | Split a module declaration in half by trying to remove assign
-- statements. This is only done in the main module of the source.
halveAssigns :: Replace SourceInfo
halveAssigns = combine mainModule halveModAssign

-- | Checks if a module item is needed in the module declaration.
relevantModItem :: ModDecl -> ModItem -> Bool
relevantModItem (ModDecl _ out _ _ _) (ModCA (ContAssign i _)) = i `elem` fmap _portName out
relevantModItem _ Decl{}                          = True
relevantModItem _ _                                            = False

isAssign :: Statement -> Bool
isAssign (BlockAssign _)    = True
isAssign (NonBlockAssign _) = True
isAssign _                  = False

lValName :: LVal -> [Identifier]
lValName (RegId i) = [i]
lValName (RegExpr i _) = [i]
lValName (RegSize i _) = [i]
lValName (RegConcat e) = mapMaybe getId . concat $ universe <$> e
    where
        getId (Id i) = Just i
        getId _      = Nothing

portToId :: Port -> Identifier
portToId (Port _ _ _ i) = i

paramToId :: Parameter -> Identifier
paramToId (Parameter i _) = i

findActiveWires :: ModDecl -> [Identifier]
findActiveWires m@(ModDecl _ i o _ p) = nub $ assignWires <> assignStat <> fmap portToId i <> fmap portToId o <> fmap paramToId p
    where
        assignWires = m ^.. modItems . traverse . modContAssign . contAssignNetLVal
        assignStat = concatMap lValName $ (allStat ^.. traverse . stmntBA . assignReg)
                     <> (allStat ^.. traverse . stmntNBA . assignReg)
        allStat = filter isAssign . concat $ fmap universe stat
        stat = (m ^.. modItems . traverse . _Initial) <> (m ^.. modItems . traverse . _Always)

cleanSourceInfo :: SourceInfo -> SourceInfo
cleanSourceInfo src = clean active src
    where
        active = findActiveWires (src ^. mainModule)

-- | Reducer for module items. It does a binary search on all the module items,
-- except assignments to outputs and input-output declarations.
halveModItems :: Replace SourceInfo
halveModItems srcInfo = cleanSourceInfo . addRelevant <$> src
    where
        repl = halve . filter (not . relevantModItem main)
        relevant = filter (relevantModItem main) $ main ^. modItems
        main = srcInfo ^. mainModule
        src = combine (mainModule . modItems) repl srcInfo
        addRelevant = mainModule . modItems %~ (relevant ++)

-- | Reduce expressions by splitting them in half and keeping the half that
-- succeeds.
halveExpr :: Replace SourceInfo
halveExpr = combine contexpr $ traverse halveModExpr
  where
    contexpr :: Lens' SourceInfo [ModItem]
    contexpr = mainModule . modItems

-- | Reduction using custom reduction strategies.
reduce_
    :: Replace SourceInfo
    -> (SourceInfo -> IO Bool)
    -> SourceInfo
    -> IO SourceInfo
reduce_ repl eval src = do
    replAnswer <- sequenceA $ evalIfNotEmpty <$> replacement
    case (replacement, replAnswer) of
        (Single s, Single True    ) -> runIf s
        (Dual _ r, Dual False True ) -> runIf r
        (Dual l _, Dual True False ) -> runIf l
        (Dual l r, Dual True True) -> do
            lreduced <- runIf l
            rreduced <- runIf r
            if _infoSrc lreduced < _infoSrc rreduced
                then return lreduced
                else return rreduced
        _            -> return src
  where
    replacement = repl src
    runIf s = if s /= src then reduce_ repl eval s else return s
    evalIfNotEmpty m = do
        print
            $   GenVerilog
            <$> m
            ^.. mainModule
            .   modItems
            .   traverse
            .   modContAssign
        eval m

-- | Reduce an input to a minimal representation. It follows the reduction
-- strategy mentioned above.
reduce
    :: (SourceInfo -> IO Bool) -- ^ Failed or not.
    -> SourceInfo              -- ^ Input verilog source to be reduced.
    -> IO SourceInfo           -- ^ Reduced output.
reduce eval src = red halveAssigns src >>= red halveExpr
    where red a = reduce_ a eval
