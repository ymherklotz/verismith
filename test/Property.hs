module Property
  ( propertyTests
  )
where

import           Data.Either                       (isRight)
import qualified Data.Graph.Inductive              as G
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Test.Tasty
import qualified Test.Tasty.QuickCheck             as QC
import           Text.Parsec
import           VeriFuzz
import qualified VeriFuzz.RandomAlt                as V

newtype TestGraph = TestGraph { getGraph :: Gr Gate () }
                  deriving (Show)

newtype AltTestGraph = AltTestGraph { getAltGraph :: Gr Gate () }
                  deriving (Show)

newtype ModDeclSub = ModDeclSub { getModDecl :: ModDecl }

instance Show ModDeclSub where
  show = show . GenVerilog . getModDecl

instance QC.Arbitrary ModDeclSub where
  arbitrary = ModDeclSub <$> randomMod 3 10

instance QC.Arbitrary TestGraph where
  arbitrary = TestGraph <$> QC.resize 30 randomDAG

instance QC.Arbitrary AltTestGraph where
  arbitrary = AltTestGraph <$> QC.resize 100 V.randomDAG

simpleGraph :: TestTree
simpleGraph = QC.testProperty "simple graph generation check" $ \graph -> simp graph
  where simp = G.isSimple . getGraph

simpleAltGraph :: TestTree
simpleAltGraph = QC.testProperty "simple alternative graph generation check" $ \graph -> simp graph
  where simp = G.isSimple . getAltGraph

parserInput' :: ModDeclSub -> Bool
parserInput' (ModDeclSub v) =
  isRight $ parse parseModDecl "input_test.v" str
  where
    str = show . GenVerilog $ v

--parserIdempotent' :: (GenVerilog VerilogSrc) -> Bool
--parserIdempotent' v =
--  p sv == (p . p) sv
--  where
--    sv = show v
--    p = show . fromRight (VerilogSrc []) . parse parseVerilogSrc "idempotent_test.v"

parserInput :: TestTree
parserInput = QC.testProperty "parser input" $
  parserInput'

--parserIdempotent :: TestTree
--parserIdempotent = QC.testProperty "parser idempotence" $
--  parserIdempotent'

propertyTests :: TestTree
propertyTests = testGroup "Property Tests" [simpleGraph, simpleAltGraph, parserInput]
