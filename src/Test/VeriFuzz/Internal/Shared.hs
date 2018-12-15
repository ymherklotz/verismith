{-|
Module      : Test.VeriFuzz.Internal.Shared
Description : Shared high level code used in the other modules internally.
Copyright   : (c) Yann Herklotz Grave 2018
License     : GPL-3
Maintainer  : ymherklotz@gmail.com
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

-- | Converts unsafe list functions in the Prelude to a safe version.
safe :: ([a] -> b) -> [a] -> Maybe b
safe _ [] = Nothing
safe f l  = Just $ f l
