module Test.VeriFuzz.Internal.Shared where

import           Data.Maybe (fromMaybe)

fromList :: (Foldable t, Monoid a) => t a -> a
fromList = foldl mappend mempty

sep :: (Monoid a) => a -> [a] -> a
sep el l = fromMaybe mempty $
  (fromList . fmap (<>el) <$> safe init l) <> safe last l

safe :: ([a] -> b) -> [a] -> Maybe b
safe _ [] = Nothing
safe f l  = Just $ f l
