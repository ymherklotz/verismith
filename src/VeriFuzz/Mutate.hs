{-|
Module      : VeriFuzz.Mutation
Description : Functions to mutate the Verilog AST.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Functions to mutate the Verilog AST from "VeriFuzz.Verilog.AST" to generate more
random patterns, such as nesting wires instead of creating new ones.
-}

module VeriFuzz.Mutate where

import           Control.Lens
import           Data.Maybe        (catMaybes, fromMaybe)
import           Data.Text         (Text)
import qualified Data.Text         as T
import           VeriFuzz.AST
import           VeriFuzz.Internal

-- | Return if the 'Identifier' is in a 'ModDecl'.
inPort :: Identifier -> ModDecl -> Bool
inPort i m = inInput
    where inInput = any (\a -> a ^. portName == i) $ m ^. modInPorts ++ m ^. modOutPorts

-- | Find the last assignment of a specific wire/reg to an expression, and
-- returns that expression.
findAssign :: Identifier -> [ModItem] -> Maybe Expr
findAssign i items = safe last . catMaybes $ isAssign <$> items
  where
    isAssign (ModCA (ContAssign val expr)) | val == i  = Just expr
                                           | otherwise = Nothing
    isAssign _ = Nothing

-- | Transforms an expression by replacing an Identifier with an
-- expression. This is used inside 'transformOf' and 'traverseExpr' to replace
-- the 'Identifier' recursively.
idTrans :: Identifier -> Expr -> Expr -> Expr
idTrans i expr (Id id') | id' == i  = expr
                        | otherwise = Id id'
idTrans _ _ e = e

-- | Replaces the identifier recursively in an expression.
replace :: Identifier -> Expr -> Expr -> Expr
replace = (transformOf traverseExpr .) . idTrans

-- | Nest expressions for a specific 'Identifier'. If the 'Identifier' is not
-- found, the AST is not changed.
--
-- This could be improved by instead of only using the last assignment to the
-- wire that one finds, to use the assignment to the wire before the current
-- expression. This would require a different approach though.
nestId :: Identifier -> ModDecl -> ModDecl
nestId i m
    | not $ inPort i m
    = let expr = fromMaybe def . findAssign i $ m ^. modItems in m & get %~ replace i expr
    | otherwise
    = m
  where
    get = modItems . traverse . modContAssign . contAssignExpr
    def = Id i

-- | Replaces an identifier by a expression in all the module declaration.
nestSource :: Identifier -> VerilogSrc -> VerilogSrc
nestSource i src = src & getVerilogSrc . traverse . getDescription %~ nestId i

-- | Nest variables in the format @w[0-9]*@ up to a certain number.
nestUpTo :: Int -> VerilogSrc -> VerilogSrc
nestUpTo i src = foldl (flip nestSource) src $ Identifier . fromNode <$> [1 .. i]

allVars :: ModDecl -> [Identifier]
allVars m = (m ^.. modOutPorts . traverse . portName) <> (m ^.. modInPorts . traverse . portName)

-- $setup
-- >>> import VeriFuzz.CodeGen
-- >>> let m = (ModDecl (Identifier "m") [Port Wire False 5 (Identifier "y")] [Port Wire False 5 "x"] [])
-- >>> let main = (ModDecl "main" [] [] [])

-- | Add a Module Instantiation using 'ModInst' from the first module passed to
-- it to the body of the second module. It first has to make all the inputs into
-- @reg@.
--
-- >>> render $ instantiateMod m main
-- module main;
-- wire [4:0] y;
-- reg [4:0] x;
-- m m1(y, x);
-- endmodule
-- <BLANKLINE>
instantiateMod :: ModDecl -> ModDecl -> ModDecl
instantiateMod m main = main & modItems %~ ((out ++ regIn ++ [inst]) ++)
  where
    out   = Decl Nothing <$> m ^. modOutPorts
    regIn = Decl Nothing <$> (m ^. modInPorts & traverse . portType .~ Reg)
    inst  = ModInst (m ^. modId) (m ^. modId <> (Identifier . showT $ count + 1)) conns
    count = length . filter (== m ^. modId) $ main ^.. modItems . traverse . modInstId
    conns = ModConn . Id <$> allVars m

-- | Instantiate without adding wire declarations. It also does not count the
-- current instantiations of the same module.
--
-- >>> GenVerilog $ instantiateMod_ m
-- m m(y, x);
-- <BLANKLINE>
instantiateMod_ :: ModDecl -> ModItem
instantiateMod_ m = ModInst (m ^. modId) (m ^. modId) conns
  where
    conns =
        ModConn
            .   Id
            <$> (m ^.. modOutPorts . traverse . portName)
            ++  (m ^.. modInPorts . traverse . portName)

-- | Instantiate without adding wire declarations. It also does not count the
-- current instantiations of the same module.
--
-- >>> GenVerilog $ instantiateModSpec_ "_" m
-- m m(.y(y), .x(x));
-- <BLANKLINE>
instantiateModSpec_ :: Text -> ModDecl -> ModItem
instantiateModSpec_ outChar m = ModInst (m ^. modId) (m ^. modId) conns
  where
    conns   = zipWith ModConnNamed ids (Id <$> instIds)
    ids     = filterChar outChar (name modOutPorts) <> name modInPorts
    instIds = name modOutPorts <> name modInPorts
    name v = m ^.. v . traverse . portName

filterChar :: Text -> [Identifier] -> [Identifier]
filterChar t ids =
    ids & traverse . getIdentifier %~ (\x -> fromMaybe x . safe head $ T.splitOn t x)

-- | Initialise all the inputs and outputs to a module.
--
-- >>> GenVerilog $ initMod m
-- module m(y, x);
-- output wire [4:0] y;
-- input wire [4:0] x;
-- endmodule
-- <BLANKLINE>
initMod :: ModDecl -> ModDecl
initMod m = m & modItems %~ ((out ++ inp) ++)
  where
    out = Decl (Just PortOut) <$> (m ^. modOutPorts)
    inp = Decl (Just PortIn) <$> (m ^. modInPorts)

-- | Make an 'Identifier' from and existing Identifier and an object with a
-- 'Show' instance to make it unique.
makeIdFrom :: (Show a) => a -> Identifier -> Identifier
makeIdFrom a i = (i <>) . Identifier . ("_" <>) $ showT a

-- | Make top level module for equivalence verification. Also takes in how many
-- modules to instantiate.
makeTop :: Int -> ModDecl -> ModDecl
makeTop i m = ModDecl (m ^. modId) ys (m ^. modInPorts) modIt
  where
    ys    = yPort . flip makeIdFrom "y" <$> [1 .. i]
    modIt = instantiateModSpec_ "_" . modN <$> [1 .. i]
    modN n = m & modId %~ makeIdFrom n & modOutPorts .~ [yPort (makeIdFrom n "y")]

-- | Make a top module with an assert that requires @y_1@ to always be equal to
-- @y_2@, which can then be proven using a formal verification tool.
makeTopAssert :: ModDecl -> ModDecl
makeTopAssert = (modItems %~ (++ [assert])) . (modInPorts %~ addClk) . makeTop 2
  where
    assert = Always . EventCtrl e . Just $ SeqBlock
        [TaskEnable $ Task "assert" [BinOp (Id "y_1") BinEq (Id "y_2")]]
    e      = EPosEdge "clk"
    addClk = (defaultPort "clk" :)

-- | Provide declarations for all the ports that are passed to it.
declareMod :: [Port] -> ModDecl -> ModDecl
declareMod ports = modItems %~ (decl ++) where decl = Decl Nothing <$> ports

-- | Simplify an 'Expr' by using constants to remove 'BinaryOperator' and
-- simplify expressions. To make this work effectively, it should be run until
-- no more changes were made to the expression.
--
-- >>> GenVerilog . simplify $ (Id "x") + 0
-- x
--
-- >>> GenVerilog . simplify $ (Id "y") + (Id "x")
-- (y + x)
simplify :: Expr -> Expr
simplify (BinOp (Number _ 1) BinAnd e)   = e
simplify (BinOp e BinAnd (Number _ 1))   = e
simplify (BinOp (Number _ 0) BinAnd _)   = Number 1 0
simplify (BinOp _ BinAnd (Number _ 0))   = Number 1 0
simplify (BinOp e BinPlus (Number _ 0))  = e
simplify (BinOp (Number _ 0) BinPlus e)  = e
simplify (BinOp e BinMinus (Number _ 0)) = e
simplify (BinOp (Number _ 0) BinMinus e) = e
simplify (BinOp e BinTimes (Number _ 1)) = e
simplify (BinOp (Number _ 1) BinTimes e) = e
simplify (BinOp _ BinTimes (Number _ 0)) = Number 1 0
simplify (BinOp (Number _ 0) BinTimes _) = Number 1 0
simplify (BinOp e BinOr (Number _ 0))    = e
simplify (BinOp (Number _ 0) BinOr e)    = e
simplify (BinOp e BinLSL (Number _ 0))   = e
simplify (BinOp (Number _ 0) BinLSL e)   = e
simplify (BinOp e BinLSR (Number _ 0))   = e
simplify (BinOp (Number _ 0) BinLSR e)   = e
simplify (BinOp e BinASL (Number _ 0))   = e
simplify (BinOp (Number _ 0) BinASL e)   = e
simplify (BinOp e BinASR (Number _ 0))   = e
simplify (BinOp (Number _ 0) BinASR e)   = e
simplify (UnOp UnPlus e)                 = e
simplify e                               = e

-- | Remove all 'Identifier' that do not appeare in the input list from an
-- 'Expr'. The identifier will be replaced by @1'b0@, which can then later be
-- simplified further.
--
-- >>> GenVerilog . removeId ["x"] $ Id "x" + Id "y"
-- (x + (1'h0))
removeId :: [Identifier] -> Expr -> Expr
removeId i = transform trans
  where
    trans (Id ident) | ident `notElem` i = Number 1 0
                     | otherwise         = Id ident
    trans e = e
