{-|
Module      : Test.VeriFuzz.Internal.Shared
Description : Shared high level code used in the other modules internally.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Shared high level code used in the other modules internally.
-}

module Test.VeriFuzz.Internal.Shared where

import           Data.Maybe (fromMaybe)

-- | Fold up a list of Monoids using mappend and mempty as the first
-- element.
fromList :: (Foldable t, Monoid a) => t a -> a
fromList = foldl mappend mempty

-- | Combine the Monoid elements of a list and insert the seperation symbol in
-- between each element except the last one.
sep :: (Monoid a) => a -> [a] -> a
sep el l = fromMaybe mempty $
  (fromList . fmap (<>el) <$> safe init l) <> safe last l

-- | Alternative sep which returns the pattern if the list is empty.
sep_ :: (Monoid a) => a -> [a] -> a
sep_ el l
  | null l = mempty
  | otherwise = el <> sep el l

-- | Converts unsafe list functions in the Prelude to a safe version.
safe :: ([a] -> b) -> [a] -> Maybe b
safe _ [] = Nothing
safe f l  = Just $ f l
