module Property (propertyTests) where

import           Data.Graph.Inductive
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC
import           Test.VeriFuzz

newtype TestGraph = TestGraph { getGraph :: Gr Gate ()}
                  deriving (Show)

instance QC.Arbitrary TestGraph where
  arbitrary = TestGraph <$> randomDAG 100

simpleGraph = QC.testProperty "simple graph generation" $
  \graph -> isSimple (getGraph (graph :: TestGraph)) == True

propertyTests :: TestTree
propertyTests = testGroup "Property"
  [ simpleGraph
  ]
