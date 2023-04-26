module Distance
  ( distanceTests,
  )
where

import Hedgehog (Property, (===))
import qualified Hedgehog as Hog
import qualified Hedgehog.Gen as Hog
import qualified Hedgehog.Range as Hog
import Test.Tasty
import Test.Tasty.Hedgehog
import Verismith.Verilog.Distance

distanceLess :: Property
distanceLess = Hog.property $ do
  x <- Hog.forAll (Hog.list (Hog.linear 0 15) Hog.alpha)
  y <- Hog.forAll (Hog.list (Hog.linear 0 15) Hog.alpha)
  Hog.assert $ udistance x y <= distance x y

distanceEq :: Property
distanceEq = Hog.property $ do
  x <- Hog.forAll (Hog.list (Hog.linear 0 15) Hog.alpha)
  distance x x === 0
  udistance x x === 0

distanceTests :: TestTree
distanceTests =
  testGroup
    "Distance tests"
    [ testProperty "Unordered distance <= distance" distanceLess,
      testProperty "distance x x === 0" distanceEq
    ]
