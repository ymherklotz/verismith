module Property (propertyTests) where

import           Data.Graph.Inductive
import           Test.Tasty
import qualified Test.Tasty.QuickCheck         as QC
import           Test.VeriFuzz
import qualified Test.VeriFuzz.Graph.RandomAlt as V

newtype TestGraph = TestGraph { getGraph :: Gr Gate () }
                  deriving (Show)

newtype AltTestGraph = AltTestGraph { getAltGraph :: Gr Gate () }
                  deriving (Show)

instance QC.Arbitrary TestGraph where
  arbitrary = TestGraph <$> QC.resize 30 randomDAG

instance QC.Arbitrary AltTestGraph where
  arbitrary = AltTestGraph <$> QC.resize 100 V.randomDAG

simpleGraph = QC.testProperty "simple graph generation check" $
  \graph -> simp graph
  where simp = isSimple . getGraph

simpleAltGraph = QC.testProperty "simple alternative graph generation check" $
  \graph -> simp graph
  where simp = isSimple . getAltGraph

propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
  [ simpleGraph
  , simpleAltGraph
  ]
