module Unit (unitTests) where

import           Control.Lens
import qualified Data.Graph.Inductive as G
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Test.Tasty
import           Test.Tasty.HUnit
import           VeriFuzz

unitTests = testGroup "Unit tests"
  [ testCase "Transformation of AST" $
      assertEqual "Successful transformation" transformExpectedResult
      (transformOf traverseExpr trans transformTestData)
  , testCase ""
  ]

transformTestData :: Expr
transformTestData = BinOp (BinOp (BinOp (Id "id1") BinAnd (Id "id2")) BinAnd
                            (BinOp (Id "id1") BinAnd (Id "id2"))) BinAnd
                    (BinOp (BinOp (BinOp (Id "id1") BinAnd (Id "id2")) BinAnd
                     (BinOp (Id "id1") BinAnd (BinOp (BinOp (Id "id1") BinAnd (Id "id2")) BinAnd
                                                      (BinOp (Id "id1") BinAnd (Id "id2"))))) BinOr
                    (Concat [Concat [ Concat [Id "id1", Id "id2", Id "id2"], Id "id2", Id "id2"
                                    , Concat [Id "id2", Id "id2", Concat [Id "id1", Id "id2"]]
                                    , Id "id2"], Id "id1", Id "id2"]))

transformExpectedResult :: Expr
transformExpectedResult = BinOp (BinOp (BinOp (Id "id1") BinAnd (Id "Replaced")) BinAnd
                            (BinOp (Id "id1") BinAnd (Id "Replaced"))) BinAnd
                    (BinOp (BinOp (BinOp (Id "id1") BinAnd (Id "Replaced")) BinAnd
                     (BinOp (Id "id1") BinAnd (BinOp (BinOp (Id "id1") BinAnd (Id "Replaced")) BinAnd
                                                      (BinOp (Id "id1") BinAnd (Id "Replaced"))))) BinOr
                    (Concat [Concat [ Concat [Id "id1", Id "Replaced", Id "Replaced"], Id "Replaced", Id "Replaced"
                                    , Concat [Id "Replaced", Id "Replaced", Concat [Id "id1", Id "Replaced"]]
                                    , Id "Replaced"], Id "id1", Id "Replaced"]))

trans e =
  case e of
    Id id -> if id == Identifier "id2" then
                              Id $ Identifier "Replaced"
                            else Id id
    _                    -> e
