module Distance
  ( distanceTests
  )
where

import Hedgehog (Property)
import qualified Hedgehog as Hog
import qualified Hedgehog.Gen as Hog
import qualified Hedgehog.Range as Hog
import Verismith.Verilog.Distance
import Test.Tasty
import Test.Tasty.Hedgehog

distanceLess :: Property
distanceLess = Hog.property $ do
  x <- Hog.forAll (Hog.list (Hog.linear 0 10) Hog.alpha)
  y <- Hog.forAll (Hog.list (Hog.linear 0 10) Hog.alpha)
  Hog.assert $ udistance x y <= distance x y

distanceTests :: TestTree
distanceTests = testProperty "Unordered distance <= distance" distanceLess
