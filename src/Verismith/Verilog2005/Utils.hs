-- Module      : Verismith.Verilog2005.Utils
-- Description : AST utilitary functions.
-- Copyright   : (c) 2023 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : experimental
-- Portability : POSIX

module Verismith.Verilog2005.Utils
  ( nonEmpty,
    foldrMap1,
    foldrMap1',
    foldrMapM1,
    rmapM,
    nermapM,
    rmap',
    rmap,
    liftA4,
    liftA5,
    mkpair,
    stmtDanglingElse,
    genDanglingElse,
    getModuleParamTopNames,
  )
where

import Control.Applicative
import Control.Monad.Fail
import qualified Data.ByteString as BS
import qualified Data.HashSet as HS
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Verismith.Verilog2005.AST

-- List and nonempty list utils

nonEmpty :: b -> (NonEmpty a -> b) -> [a] -> b
nonEmpty e ne = maybe e ne . NE.nonEmpty

foldrMap1 :: (a -> b) -> (a -> b -> b) -> NonEmpty a -> b
foldrMap1 f g (h :| t) = nonEmpty (f h) (\x -> g h $ foldrMap1 f g x) t

foldrMap1' :: b -> (a -> b) -> (a -> b -> b) -> [a] -> b
foldrMap1' d f g = nonEmpty d (foldrMap1 f g)

foldrMapM1 :: (Applicative m, Monad m) => (a -> m b) -> (a -> b -> m b) -> NonEmpty a -> m b
foldrMapM1 f g (h :| t) = nonEmpty (f h) (\x -> foldrMapM1 f g x >>= g h) t

rmapM :: Monad m => (a -> m b) -> [a] -> m [b]
rmapM f = foldl' (\acc e -> do y <- acc; x <- f e; return $ x : y) $ return []

nermapM :: MonadFail m => (a -> m b) -> NonEmpty a -> m (NonEmpty b)
nermapM f (x :| l) =
  foldl' (\acc e -> do y <- acc; x <- f e; return $ x <| y) ((:| []) <$> f x) l

rmap' :: (a -> b) -> [b] -> [a] -> [b]
rmap' f = foldl' (\acc e -> f e : acc)

rmap :: (a -> b) -> [a] -> [b]
rmap f = rmap' f []

liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = liftA3 f a b c <*> d

liftA5 :: Applicative f => (a -> b -> c -> d -> e -> z) -> f a -> f b -> f c -> f d -> f e -> f z
liftA5 f a b c d e = liftA4 f a b c d <*> e

mkpair :: Applicative f => f a -> f b -> f (a, b)
mkpair = liftA2 (,)

-- AST utils

-- | Checks if there is a dangling else
stmtDanglingElse :: MybStmt -> MybStmt -> Bool
stmtDanglingElse fb (Attributed _ tb) = case tb of
  Just (SProcTimingControl _ s) -> stmtDanglingElse fb s
  Just (SWait _ s) -> stmtDanglingElse fb s
  Just (SLoop _ (Attributed a s)) -> stmtDanglingElse fb $ Attributed a $ Just s
  Just (SIf _ _ tfb) -> (fb == Attributed [] Nothing) /= (tfb == Attributed [] Nothing)
  _ -> False

genDanglingElse :: Maybe GenerateBlock -> Maybe GenerateBlock -> Bool
genDanglingElse fb tb = case tb of
  Just (GBSingle (GIMGI (Attributed _ (MGILoopGen _ _ _ s) :| []))) -> genDanglingElse fb $ Just s
  Just (GBSingle (GIMGI (Attributed _ (MGIIf _ _ tfb) :| []))) -> (fb == Nothing) /= (tfb == Nothing)
  _ -> False

addGBParamTopNames :: HS.HashSet BS.ByteString -> GenerateBlock -> HS.HashSet BS.ByteString
addGBParamTopNames s mgi = case mgi of
  GBSingle gi -> case gi of
    GIParam lp -> foldl' (\s p -> HS.insert (_ParamIdent p) s) s lp
    GIMGD lmgd -> foldl' addMGDParamTopNames s lmgd
    GIMGI lmgi -> foldl' (\s (Attributed _ mgi) -> addMGIParamTopNames s mgi) s lmgi
    _ -> s
  GBBlock (Identified n gr) -> if BS.null n then addGRParamTopNames s gr else HS.insert n s

addGRParamTopNames :: HS.HashSet BS.ByteString -> GenerateRegion -> HS.HashSet BS.ByteString
addGRParamTopNames s (GenerateRegion _ _ d b) =
  foldl' addMGDParamTopNames (foldl' (\s (Attributed _ mgi) -> addMGIParamTopNames s mgi) s b) d

addMGIParamTopNames :: HS.HashSet BS.ByteString -> ModGenItem -> HS.HashSet BS.ByteString
addMGIParamTopNames s mgi = case mgi of
  MGIModInst _ _ n _ _ -> HS.insert n s
  MGIUnknownInst _ _ n _ _ _ -> HS.insert n s
  MGILoopGen _ _ _ gb -> addGBParamTopNames s gb
  MGIIf _ tgb fgb -> fromMGB (fromMGB s fgb) tgb
  MGICase _ lgci mgb -> foldl' (\s (GenCaseItem _ mgb) -> fromMGB s mgb) (fromMGB s mgb) lgci
  _ -> s
  where
    fromMGB s mgb = case mgb of
      Nothing -> s
      Just gb -> addGBParamTopNames s gb

addMGDParamTopNames :: HS.HashSet BS.ByteString -> AttrIded ModGenDecl -> HS.HashSet BS.ByteString
addMGDParamTopNames s (AttrIded _ n mgd) = case mgd of
  MGDTask _ _ _ _ _ _ -> HS.insert n s
  MGDFunc _ _ _ _ _ _ _ -> HS.insert n s
  _ -> s

getModuleParamTopNames :: ModuleBlock -> HS.HashSet BS.ByteString
getModuleParamTopNames (ModuleBlock _ _ _ _ p _ d _ b _ _ _ _) =
  foldl'
    (\s p -> HS.insert (_ParamIdent p) s)
    ( foldl'
        addMGDParamTopNames
        ( foldl'
            ( \s mi -> case mi of
                MIMGI (Attributed _ mgi) -> addMGIParamTopNames s mgi
                MIGenReg gr -> addGRParamTopNames s gr
                _ -> s
            )
            HS.empty
            b
        )
        d
    )
    p
