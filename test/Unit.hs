module Unit
    ( unitTests
    )
where

import           Control.Lens
import           Data.List.NonEmpty (NonEmpty (..))
import           Parser             (parseUnitTests)
import           Reduce             (reduceUnitTests)
import           Test.Tasty
import           Test.Tasty.HUnit
import           VeriFuzz

unitTests :: TestTree
unitTests = testGroup
    "Unit tests"
    [ testCase "Transformation of AST" $ assertEqual
          "Successful transformation"
          transformExpectedResult
          (transform trans transformTestData)
    , parseUnitTests
    , reduceUnitTests
    ]

transformTestData :: Expr
transformTestData = BinOp
    (BinOp (BinOp (Id "id1") BinAnd (Id "id2"))
           BinAnd
           (BinOp (Id "id1") BinAnd (Id "id2"))
    )
    BinAnd
    (BinOp
        (BinOp
            (BinOp (Id "id1") BinAnd (Id "id2"))
            BinAnd
            (BinOp
                (Id "id1")
                BinAnd
                (BinOp (BinOp (Id "id1") BinAnd (Id "id2"))
                       BinAnd
                       (BinOp (Id "id1") BinAnd (Id "id2"))
                )
            )
        )
        BinOr
        (Concat $
            ( Concat $
                (Concat $ (Id "id1") :| [Id "id2", Id "id2"]) :|
                [ Id "id2"
                , Id "id2"
                , (Concat $ (Id "id2") :| [Id "id2", (Concat $ Id "id1" :| [Id "id2"])])
                , Id "id2"
                ]
            ) :| [ Id "id1"
                 , Id "id2"
                 ]
        )
    )

transformExpectedResult :: Expr
transformExpectedResult = BinOp
    (BinOp (BinOp (Id "id1") BinAnd (Id "Replaced"))
           BinAnd
           (BinOp (Id "id1") BinAnd (Id "Replaced"))
    )
    BinAnd
    (BinOp
        (BinOp
            (BinOp (Id "id1") BinAnd (Id "Replaced"))
            BinAnd
            (BinOp
                (Id "id1")
                BinAnd
                (BinOp (BinOp (Id "id1") BinAnd (Id "Replaced"))
                       BinAnd
                       (BinOp (Id "id1") BinAnd (Id "Replaced"))
                )
            )
        )
        BinOr
        (Concat $
            ( Concat $
                (Concat $ (Id "id1") :| [Id "Replaced", Id "Replaced"]) :|
                [ Id "Replaced"
                , Id "Replaced"
                , Concat $
                    Id "Replaced" :|
                    [ Id "Replaced"
                    , Concat $ Id "id1" :| [Id "Replaced"]
                    ]
                , Id "Replaced"
                ] ) :| [ Id "id1"
                       , Id "Replaced"
                       ]
        )
    )

trans :: Expr -> Expr
trans e = case e of
    Id i -> if i == Identifier "id2" then Id $ Identifier "Replaced" else Id i
    _    -> e
