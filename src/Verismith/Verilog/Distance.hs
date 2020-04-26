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

data Pair a b = Pair a b
  deriving Show

instance Eq b => Eq (Pair a b) where
  Pair _ a == Pair _ b = a == b

instance Ord b => Ord (Pair a b) where
  Pair _ a <= Pair _ b = a <= b

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
  dempty (a:b) = minimum [dempty a, dempty b]

lDistance :: ( Eq a ) => [a] -> [a] -> Int
lDistance [] t = length t   -- If s is empty the distance is the number of characters in t
lDistance s [] = length s   -- If t is empty the distance is the number of characters in s
lDistance (a:s') (b:t') =
  if
    a == b
  then
    lDistance s' t'         -- If the first characters are the same they can be ignored
  else
    1 + minimum             -- Otherwise try all three possible actions and select the best one
      [ lDistance (a:s') t' -- Character is inserted (b inserted)
      , lDistance s' (b:t') -- Character is deleted  (a deleted)
      , lDistance s' t'     -- Character is replaced (a replaced with b)
      ]

instance Distance a => Distance (Maybe a) where
  distance Nothing a = dempty a
  distance a Nothing = dempty a
  distance (Just a) (Just b) = distance a b

  udistance (Just a) (Just b) = udistance a b
  udistance a b = distance a b

  dempty Nothing = 0
  dempty (Just a) = dempty a

instance Distance Char where
  distance a b = if a == b then 0 else 1
