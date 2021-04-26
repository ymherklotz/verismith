{-|
Module      : Verismith.Verilog.Distance
Description : Definition of the distance function for the abstract syntax tree.
Copyright   : (c) 2020, Yann Herklotz
License     : GPL-3
Maintainer  : yann [at] yannherklotz [dot] com
Stability   : experimental
Poratbility : POSIX

Define the distance function for the abstract syntax tree, so that different
Verilog files can be compared.  This allows us to define a metric on how
different two pieces of Verilog are.  Currently, differences in expressions are
ignored, as these are not that interesting.
-}

module Verismith.Verilog.Distance where

import Verismith.Verilog.AST
import Verismith.Verilog.Eval
import Data.Functor.Foldable (cata)
import Data.Text (Text, unpack)

data Pair a b = Pair a b
  deriving Show

instance Eq b => Eq (Pair a b) where
  Pair _ a == Pair _ b = a == b

instance Ord b => Ord (Pair a b) where
  Pair _ a <= Pair _ b = a <= b

eqDistance :: Eq a => a -> a -> Int
eqDistance a b = if a == b then 0 else 1
{-# INLINE eqDistance #-}

emptyDistance :: a -> a -> Int
emptyDistance _ _ = 0
{-# INLINE emptyDistance #-}

class Distance a where
  distance :: a -> a -> Int
  udistance :: a -> a -> Int
  udistance = distance
  dempty :: a -> Int
  dempty _ = 1

minimumloc :: Distance a => a -> [a] -> Pair Int Int
minimumloc ah [] = Pair 0 $ dempty ah
minimumloc ah b = minimum $ (\(loc, el) -> Pair loc (udistance ah el)) <$> zip [0..] b

removeAt :: Int -> [a] -> [a]
removeAt loc lst =
  let (a, b) = splitAt loc lst in
    if null b then a else a ++ tail b

remdist :: Distance a => [a] -> [a] -> Int
remdist [] a = distance [] a
remdist a [] = distance [] a
remdist (x:xs) b
  | cost <= dx = udistance xs (removeAt loc b) + cost
  | otherwise = udistance xs b + dx
  where
    Pair loc cost = minimumloc x b
    dx = dempty x

instance Distance a => Distance [a] where
  distance [] [] = 0
  distance [] l = sum $ dempty <$> l
  distance l [] = sum $ dempty <$> l
  distance a@(ah:at) b@(bh:bt) =
    let cost = distance ah bh in
      if cost == 0 then
        distance at bt
      else
        minimum [ distance at b + dempty ah
                , distance bt a + dempty bh
                , distance at bt + cost
                ]

  udistance a b = minimum [ remdist a b
                          , remdist b a
                          ]

  dempty [] = 0
  dempty (a:b) = maximum [dempty a, dempty b]

instance Distance a => Distance (Maybe a) where
  distance Nothing a = dempty a
  distance a Nothing = dempty a
  distance (Just a) (Just b) = distance a b

  udistance (Just a) (Just b) = udistance a b
  udistance a b = distance a b

  dempty Nothing = 0
  dempty (Just a) = dempty a

instance Distance Char where
  distance = eqDistance

instance Distance Bool where
  distance = eqDistance

instance Distance Integer where
  distance = eqDistance

instance Distance Text where
  distance t1 t2 = distance (unpack t1) (unpack t2)

instance Distance Identifier where
  distance = eqDistance

eval :: ConstExpr -> Integer
eval c = toInteger (cata (evaluateConst []) c)

instance Distance ConstExpr where
  distance c1 c2 = distance (eval c1) $ eval c2
  udistance c1 c2 = udistance (eval c1) $ eval c2

instance Distance Parameter where
  distance _ _ = 0

instance Distance PortType where
  distance = eqDistance

instance Distance PortDir where
  distance = eqDistance

instance Distance (Statement a) where
  distance (TimeCtrl _ s1) (TimeCtrl _ s2) = distance s1 s2
  distance (EventCtrl _ s1) (EventCtrl _ s2) = distance s1 s2
  distance (SeqBlock s1) (SeqBlock s2) = distance s1 s2
  distance (CondStmnt _ st1 sf1) (CondStmnt _ st2 sf2) = distance st1 st2 + distance sf1 sf2
  distance (ForLoop _ _ _ s1) (ForLoop _ _ _ s2) = distance s1 s2
  distance (StmntAnn _ s1) s2 = distance s1 s2
  distance (BlockAssign _) (BlockAssign _) = 0
  distance (NonBlockAssign _) (NonBlockAssign _) = 0
  distance (TaskEnable _) (TaskEnable _) = 0
  distance (SysTaskEnable _) (SysTaskEnable _) = 0
  distance (StmntCase _ _ _ _) (StmntCase _ _ _ _) = 0
  distance _ _ = 1

instance Distance (ModItem a) where
  distance (ModCA _) (ModCA _) = 0
  distance (ModInst _ _ _ _) (ModInst _ _ _ _) = 0
  distance (Initial _) (Initial _) = 0
  distance (Always s1) (Always s2) = distance s1 s2
  distance (Decl _ _ _) (Decl _ _ _) = 0
  distance (ParamDecl _) (ParamDecl _) = 0
  distance (LocalParamDecl _) (LocalParamDecl _) = 0
  distance _ _ = 1

instance Distance Range where
  distance (Range a1 b1) (Range a2 b2) =
    distance a1 a2 + distance b1 b2
  udistance (Range a1 b1) (Range a2 b2) =
    udistance a1 a2 + udistance b1 b2

instance Distance Port where
  distance (Port t1 s1 r1 _) (Port t2 s2 r2 _) =
    distance t1 t2 + distance s1 s2 + distance r1 r2
  udistance (Port t1 s1 r1 _) (Port t2 s2 r2 _) =
    udistance t1 t2 + udistance s1 s2 + udistance r1 r2
  dempty (Port t1 s1 r1 _) = 1 + dempty t1 + dempty s1 + dempty r1

instance Distance (ModDecl a) where
  distance (ModDecl _ min1 mout1 mis1 mp1) (ModDecl _ min2 mout2 mis2 mp2) =
    distance min1 min2 + distance mout1 mout2 + distance mis1 mis2 + distance mp1 mp2
  udistance (ModDecl _ min1 mout1 mis1 mp1) (ModDecl _ min2 mout2 mis2 mp2) =
    udistance min1 min2 + udistance mout1 mout2 + udistance mis1 mis2 + udistance mp1 mp2
  dempty (ModDecl _ min mout mis mp) = 1 + dempty min + dempty mout + dempty mis + dempty mp

instance Distance (Verilog a) where
  distance (Verilog m1) (Verilog m2) = distance m1 m2
  udistance (Verilog m1) (Verilog m2) = udistance m1 m2
  dempty (Verilog m) = 1 + dempty m

instance Distance (SourceInfo a) where
  distance (SourceInfo _ v1) (SourceInfo _ v2) = distance v1 v2
  udistance (SourceInfo _ v1) (SourceInfo _ v2) = udistance v1 v2
  dempty (SourceInfo _ v) = 1 + dempty v
