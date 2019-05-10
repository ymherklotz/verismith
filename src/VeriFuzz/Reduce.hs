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
    )
where

import           Control.Lens
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
                   deriving (Eq)

instance Show a => Show (Replacement a) where
    show None       = "None"
    show (Single a) = "--- Only try:\n" <> show a
    show (Dual a b) = "--- Try:\n" <> show a <> "\n--- Then:\n" <> show b

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
halve :: [a] -> Replacement [a]
halve []  = None
halve [_] = Single []
halve l   = Dual a b where (a, b) = splitAt (length l `div` 2) l

-- | When given a Lens and a function that works on a lower replacement, it will
-- go down, apply the replacement, and return a replacement of the original
-- module.
combine :: Lens' a b -> (b -> Replacement b) -> a -> Replacement a
combine l f i = modify <$> f (i ^. l) where modify res = i & l .~ res

-- | Deletes Id 'Expr' if they are not part of the current scope, and replaces
-- these by 0.
filterExpr :: [Identifier] -> Expr -> Expr
filterExpr ids (Id i) = if i `notElem` ids then Number 0 else Id i
filterExpr _   e      = e

-- | Checks if a declaration is part of the current scope. If not, it returns
-- 'False', otherwise 'True', as it should be kept.
filterDecl :: [Identifier] -> ModItem -> Bool
filterDecl ids (Decl Nothing (Port _ _ _ i) _) = i `elem` ids
filterDecl _   _                               = True

-- | Checks if a continuous assignment is in the current scope, if not, it
-- returns 'False'.
filterAssigns :: [Port] -> ModItem -> Bool
filterAssigns out (ModCA (ContAssign i _)) =
    elem i $ out ^.. traverse . portName
filterAssigns _ _ = True

cleanUndefined :: [Identifier] -> [ModItem] -> [ModItem]
cleanUndefined ids mis =
    filter (filterDecl usedWires) mis
        &  traverse
        .  modContAssign
        .  contAssignExpr
        %~ transform (filterExpr usedWires)
  where
    usedWires = mis ^.. traverse . modContAssign . contAssignNetLVal <> ids

halveModAssign :: ModDecl -> Replacement ModDecl
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

-- | Split a module declaration in half by trying to remove assign statements.
halveAssigns :: SourceInfo -> Replacement SourceInfo
halveAssigns = combine mainModule halveModAssign

halveIndExpr :: Expr -> Replacement Expr
halveIndExpr (Concat l      ) = Concat <$> halve l
halveIndExpr (BinOp e1 _  e2) = Dual e1 e2
halveIndExpr (Cond  _  e1 e2) = Dual e1 e2
halveIndExpr (UnOp _ e      ) = Single e
halveIndExpr (Appl _ e      ) = Single e
halveIndExpr e                = Single e

halveModExpr :: ModItem -> Replacement ModItem
halveModExpr (ModCA ca) = ModCA <$> combine contAssignExpr halveIndExpr ca
halveModExpr a          = Single a

halveExpr :: SourceInfo -> Replacement SourceInfo
halveExpr = combine contexpr $ traverse halveModExpr
  where
    contexpr :: Lens' SourceInfo [ModItem]
    contexpr = mainModule . modItems

-- $setup
-- >>> import VeriFuzz.Verilog.CodeGen
-- >>> let m = initMod $ ModDecl (Identifier "m") [Port Wire False 5 (Identifier "y")] [Port Wire False 5 "x"] [] []
-- >>> let m2 = m & modId .~ "m2"
-- >>> let inst = ModInst "m" "m" [ModConn (Id "y"), ModConn (Id "x")]
-- >>> let inst2 = ModInst "m2" "m2" [ModConn (Id "y"), ModConn (Id "x")]
-- >>> let srcInfo = SourceInfo "top" (Verilog [(m & (modId .~ "top") . (modItems %~ (++[inst]))), m])
-- >>> let srcInfo2 = SourceInfo "top" (Verilog [(m & (modId .~ "top") . (modItems %~ (++[inst, inst2]))), m, m2])

-- | Removes half the modules randomly, until it reaches a minimal amount of
-- modules. This is done by doing a binary search on the list of modules and
-- removing the instantiations from the main module body.
--
-- >>> GenVerilog srcInfo
-- module top(y, x);
--   output wire [(3'h4):(1'h0)] y;
--   input wire [(3'h4):(1'h0)] x;
--   m m(y, x);
-- endmodule
-- <BLANKLINE>
-- module m(y, x);
--   output wire [(3'h4):(1'h0)] y;
--   input wire [(3'h4):(1'h0)] x;
-- endmodule
-- <BLANKLINE>
-- <BLANKLINE>
--
-- >>> GenVerilog <$> halveModules srcInfo
-- --- Only try:
-- module top(y, x);
--   output wire [(3'h4):(1'h0)] y;
--   input wire [(3'h4):(1'h0)] x;
-- endmodule
-- <BLANKLINE>
-- <BLANKLINE>
--
-- >>> GenVerilog srcInfo2
-- module top(y, x);
--   output wire [(3'h4):(1'h0)] y;
--   input wire [(3'h4):(1'h0)] x;
--   m m(y, x);
--   m2 m2(y, x);
-- endmodule
-- <BLANKLINE>
-- module m(y, x);
--   output wire [(3'h4):(1'h0)] y;
--   input wire [(3'h4):(1'h0)] x;
-- endmodule
-- <BLANKLINE>
-- module m2(y, x);
--   output wire [(3'h4):(1'h0)] y;
--   input wire [(3'h4):(1'h0)] x;
-- endmodule
-- <BLANKLINE>
-- <BLANKLINE>
--
-- >>> GenVerilog <$> halveModules srcInfo2
-- --- Try:
-- module top(y, x);
--   output wire [(3'h4):(1'h0)] y;
--   input wire [(3'h4):(1'h0)] x;
--   m m(y, x);
-- endmodule
-- <BLANKLINE>
-- module m(y, x);
--   output wire [(3'h4):(1'h0)] y;
--   input wire [(3'h4):(1'h0)] x;
-- endmodule
-- <BLANKLINE>
-- <BLANKLINE>
-- --- Then:
-- module top(y, x);
--   output wire [(3'h4):(1'h0)] y;
--   input wire [(3'h4):(1'h0)] x;
--   m2 m2(y, x);
-- endmodule
-- <BLANKLINE>
-- module m2(y, x);
--   output wire [(3'h4):(1'h0)] y;
--   input wire [(3'h4):(1'h0)] x;
-- endmodule
-- <BLANKLINE>
-- <BLANKLINE>
halveModules :: SourceInfo -> Replacement SourceInfo
halveModules srcInfo@(SourceInfo top _) =
    cleanModInst . addMod main <$> combine (infoSrc . _Wrapped) repl srcInfo
    where
        repl = halve . filter (not . matchesModName top)
        main = srcInfo ^. mainModule

-- | Remove all the undefined mod instances.
cleanModInst :: SourceInfo -> SourceInfo
cleanModInst srcInfo = srcInfo & infoSrc . _Wrapped .~ cleaned
    where
        validInst = srcInfo ^.. infoSrc . _Wrapped . traverse . modId
        cleaned = cleanModInst' validInst <$> srcInfo ^. infoSrc . _Wrapped

cleanModInst' :: [Identifier] -> ModDecl -> ModDecl
cleanModInst' ids m = m & modItems .~ newModItem
    where
        newModItem = filter (validModInst ids) $ m ^.. modItems . traverse

validModInst :: [Identifier] -> ModItem -> Bool
validModInst ids (ModInst i _ _) = i `elem` ids
validModInst _ _                 = True

-- | Adds a 'ModDecl' to a 'SourceInfo'.
addMod :: ModDecl -> SourceInfo -> SourceInfo
addMod m srcInfo = srcInfo & infoSrc . _Wrapped %~ (m :)

-- | Returns true if the text matches the name of a module.
matchesModName :: Text -> ModDecl -> Bool
matchesModName top (ModDecl i _ _ _ _) = top == getIdentifier i

-- | Reduction using custom reduction strategies.
reduce_
    :: (SourceInfo -> Replacement SourceInfo)
    -> (SourceInfo -> IO Bool)
    -> SourceInfo
    -> IO SourceInfo
reduce_ repl eval src = do
    replAnswer <- sequenceA $ evalIfNotEmpty <$> replacement
    case (replacement, replAnswer) of
        (Single s, Single True    ) -> runIf s
        (Dual _ l, Dual False True ) -> runIf l
        (Dual r _, Dual True False ) -> runIf r
        (Dual r l, Dual True True) -> do
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
