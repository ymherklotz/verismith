module Unit (unitTests) where

import           Control.Lens
import qualified Data.Graph.Inductive as G
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.VeriFuzz

unitTests = testGroup "Unit tests"
  [ testCase "Transformation of AST" $
      assertEqual "Successful transformation" transformExpectedResult
      (transformOf traverseExpr trans transformTestData)
  ]

primExpr :: Text -> Expression
primExpr = PrimExpr . PrimId . Identifier

transformTestData :: Expression
transformTestData = OpExpr (OpExpr (OpExpr (primExpr "id1") BinAnd (primExpr "id2")) BinAnd
                            (OpExpr (primExpr "id1") BinAnd (primExpr "id2"))) BinAnd
                    (OpExpr (OpExpr (primExpr "id1") BinAnd (primExpr "id2")) BinAnd
                     (OpExpr (primExpr "id1") BinAnd (OpExpr (OpExpr (primExpr "id1") BinAnd (primExpr "id2")) BinAnd
                                                      (OpExpr (primExpr "id1") BinAnd (primExpr "id2")))))

transformExpectedResult :: Expression
transformExpectedResult = OpExpr (OpExpr (OpExpr (primExpr "id1") BinAnd (primExpr "Replaced")) BinAnd
                                  (OpExpr (primExpr "id1") BinAnd (primExpr "Replaced"))) BinAnd
                          (OpExpr (OpExpr (primExpr "id1") BinAnd (primExpr "Replaced")) BinAnd
                           (OpExpr (primExpr "id1") BinAnd (OpExpr (OpExpr (primExpr "id1") BinAnd
                                                                    (primExpr "Replaced")) BinAnd
                                                            (OpExpr (primExpr "id1") BinAnd (primExpr "Replaced")))))

trans e =
  case e of
    PrimExpr (PrimId id) -> if id == Identifier "id2" then
                              PrimExpr . PrimId $ Identifier "Replaced"
                            else PrimExpr (PrimId id)
    _                    -> e
