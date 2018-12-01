module Test.VeriFuzz.Internal.Shared where

fromList :: (Foldable t, Monoid a) => t a -> a
fromList = foldl mappend mempty
