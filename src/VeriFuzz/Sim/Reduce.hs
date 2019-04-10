{-|
Module      : VeriFuzz.Sim.Reduce
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

module VeriFuzz.Sim.Reduce
    ( reduce
    )
where

import           Control.Lens
import           VeriFuzz.Sim.Internal
import           VeriFuzz.Verilog.AST
import           VeriFuzz.Verilog.CodeGen
import           VeriFuzz.Verilog.Mutate

data Replacement a = Dual a a
                   | Single a
                   | None
                   deriving (Eq, Show)

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
halve [a] = Single [a]
halve l   = Dual a b where (a, b) = splitAt (length l `div` 2) l

combine :: Lens' a b -> (b -> Replacement b) -> a -> Replacement a
combine l f i = modify <$> f (i ^. l) where modify res = i & l .~ res

filterExpr :: [Identifier] -> Expr -> Expr
filterExpr ids (Id i) = if i `notElem` ids then Number 1 0 else Id i
filterExpr _   e      = e

filterDecl :: [Identifier] -> ModItem -> Bool
filterDecl ids (Decl Nothing (Port _ _ _ i) _) = i `elem` ids
filterDecl _   _                               = True

filterAssigns :: [Port] -> ModItem -> Bool
filterAssigns out (ModCA (ContAssign i _)) =
    notElem i $ out ^.. traverse . portName
filterAssigns _ _ = False

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

-- | Split a module declaration in half by trying to remove assign statements.
halveAssigns :: SourceInfo -> Replacement SourceInfo
halveAssigns = combine mainModule halveModAssign

halveIndExpr :: Expr -> Replacement Expr
halveIndExpr (Concat l      ) = Concat <$> halve l
halveIndExpr (BinOp e1 _  e2) = Dual e1 e2
halveIndExpr (Cond  _  e1 e2) = Dual e1 e2
halveIndExpr (UnOp _ e      ) = Single e
halveIndExpr (Func _ e      ) = Single e
halveIndExpr e                = Single e

halveModExpr :: ModItem -> Replacement ModItem
halveModExpr (ModCA ca) = ModCA <$> combine contAssignExpr halveIndExpr ca
halveModExpr a          = Single a

halveExpr :: SourceInfo -> Replacement SourceInfo
halveExpr = combine contexpr $ traverse halveModExpr
  where
    contexpr :: Lens' SourceInfo [ModItem]
    contexpr = mainModule . modItems

reduce_
    :: (SourceInfo -> Replacement SourceInfo)
    -> (SourceInfo -> IO Bool)
    -> SourceInfo
    -> IO SourceInfo
reduce_ repl eval src = do
    replAnswer <- sequenceA $ evalIfNotEmpty <$> replacement
    case (replacement, replAnswer) of
        (Single s, Single False    ) -> runIf s
        (Dual _ l, Dual True False ) -> runIf l
        (Dual r _, Dual False True ) -> runIf r
        (Dual r l, Dual False False) -> do
            lreduced <- runIf l
            rreduced <- runIf r
            if runSource lreduced < runSource rreduced
                then return lreduced
                else return rreduced
        (None, None) -> return src
        _            -> return src
  where
    replacement = repl src
    runIf s = if s /= src then reduce eval s else return s
    evalIfNotEmpty m = do
        print
            $   GenVerilog
            <$> m
            ^.. mainModule
            .   modItems
            .   traverse
            .   modContAssign
        eval m

-- | Reduce an input to a minimal representation.
reduce
    :: (SourceInfo -> IO Bool) -- ^ Failed or not.
    -> SourceInfo              -- ^ Input verilog source to be reduced.
    -> IO SourceInfo           -- ^ Reduced output.
reduce eval src = reduce_ halveAssigns eval src >>= reduce_ halveExpr eval
