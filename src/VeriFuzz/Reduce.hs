{-|
Module      : VeriFuzz.Reduce
Description : Test case reducer implementation.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Test case reducer implementation.
-}

module VeriFuzz.Reduce
    ( reduce
    )
where

import           Control.Lens
import           VeriFuzz.AST
import           VeriFuzz.CodeGen
import           VeriFuzz.Internal
import           VeriFuzz.Mutate

data Replacement a = Dual a a
                   | Single a
                   | None
                   deriving (Eq, Show)

instance Functor Replacement where
    fmap f (Dual a b) = Dual (f a) $ f b
    fmap f (Single a) = Single $ f a
    fmap _ None       = None

instance Foldable Replacement where
    foldMap _ None       = mempty
    foldMap f (Single a) = f a
    foldMap f (Dual a b) = f a <> f b

instance Traversable Replacement where
    traverse _ None       = pure None
    traverse f (Single a) = Single <$> f a
    traverse f (Dual a b) = Dual <$> f a <*> f b

-- | Split a list in two halves.
halve :: [a] -> ([a], [a])
halve l = splitAt (length l `div` 2) l

filterExpr :: [Identifier] -> Expr -> Expr
filterExpr ids (Id i) = if i `notElem` ids then Number 1 0 else Id i
filterExpr _ e        = e

filterDecl :: [Identifier] -> ModItem -> Bool
filterDecl ids (Decl Nothing (Port _ _ _ i)) = i `elem` ids
filterDecl _   _                             = True

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
    where usedWires = mis ^.. traverse . modContAssign . contAssignNetLVal <> ids

halveModAssign :: (([ModItem], [ModItem]) -> [ModItem]) -> ModDecl -> ModDecl
halveModAssign choose m = m & modItems %~ assigns
  where
    assigns l =
        cleanUndefined (m ^.. modInPorts . traverse . portName)
            . combineAssigns (head $ m ^. modOutPorts)
            . (filter (not . filterAssigns []) l <>)
            . choose
            . halve
            . filter (filterAssigns $ m ^. modOutPorts)
            $ l

-- | Split a module declaration in half by trying to remove assign statements.
halveAssigns :: VerilogSrc -> Replacement VerilogSrc
halveAssigns vsrc =
    Dual (modified fst) (modified snd)
    where
        modified f = vsrc & getModule %~ halveModAssign f

--halveExpr :: Expr -> Replacement Expr
--halveExpr (Concat (x:xs)) = Dual x $ Concat xs
--halveExpr (BinOp e1 _ e2) = Dual e1 e2
--halveExpr (Cond _ e1 e2)  = Dual e1 e2
--halveExpr (UnOp _ e)      = Single e
--halveExpr (Func _ e)      = Single e
--halveExpr _               = None

-- | Reduce an input to a minimal representation.
reduce
    :: (SourceInfo -> IO Bool) -- ^ Failed or not.
    -> SourceInfo              -- ^ Input verilog source to be reduced.
    -> IO SourceInfo           -- ^ Reduced output.
reduce eval srcInfo@(SourceInfo top src) = do
    replAnswer <- sequenceA $ evalIfNotEmpty <$> replacement
    case (replacement, replAnswer) of
        (Single s, Single False) ->
            reduce eval $ srcTop s
        (Dual _ l, Dual True False) ->
            reduce eval $ srcTop l
        (Dual r _, Dual False True) ->
            reduce eval $ srcTop r
        (Dual r l, Dual False False) -> do
            lreduced <- reduce eval $ srcTop l
            rreduced <- reduce eval $ srcTop r
            if runSource lreduced < runSource rreduced
            then return lreduced
            else return rreduced
        _ -> return srcInfo
  where
    replacement = halveAssigns src
    srcTop = SourceInfo top
    evalIfNotEmpty m = do
        print $ GenVerilog <$> m ^.. getModule . modItems . traverse . modContAssign
        eval $ srcTop m
