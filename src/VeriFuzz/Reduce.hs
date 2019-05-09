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
    ( reduce
    )
where

import           Control.Lens
import           VeriFuzz.Verilog.AST
import           VeriFuzz.Verilog.CodeGen
import           VeriFuzz.Verilog.Mutate

-- | Replacement type that supports returning different kinds of reduced
-- replacements that could be tried.
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

-- | Reduce an input to a minimal representation. It first reduces the always
-- blocks, then reduces
reduce
    :: (SourceInfo -> IO Bool) -- ^ Failed or not.
    -> SourceInfo              -- ^ Input verilog source to be reduced.
    -> IO SourceInfo           -- ^ Reduced output.
reduce eval src = red halveAssigns src >>= red halveExpr
    where red a = reduce_ a eval
