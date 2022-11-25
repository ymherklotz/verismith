{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Verismith.Verilog.Mutate
-- Description : Functions to mutate the Verilog AST.
-- Copyright   : (c) 2018-2022, Yann Herklotz
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- Functions to mutate the Verilog AST from "Verismith.Verilog.AST" to generate more
-- random patterns, such as nesting wires instead of creating new ones.
module Verismith.Verilog.Mutate
  ( Mutate (..),
    inPort,
    findAssign,
    idTrans,
    replace,
    nestId,
    nestSource,
    nestUpTo,
    allVars,
    instantiateMod,
    instantiateMod_,
    instantiateModSpec_,
    filterChar,
    initMod,
    makeIdFrom,
    makeTop,
    makeTopAssert,
    simplify,
    removeId,
    combineAssigns,
    combineAssigns_,
    declareMod,
    fromPort,
  )
where

import Control.Lens
import Data.Foldable (fold)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Verismith.Circuit.Internal
import Verismith.Internal
import Verismith.Verilog.AST
import Verismith.Verilog.BitVec
import Verismith.Verilog.CodeGen
import Verismith.Verilog.Internal

class Mutate a where
  mutExpr :: (Expr -> Expr) -> a -> a

instance Mutate Identifier where
  mutExpr _ = id

instance Mutate Delay where
  mutExpr _ = id

instance Mutate Event where
  mutExpr f (EExpr e) = EExpr $ f e
  mutExpr _ a = a

instance Mutate BinaryOperator where
  mutExpr _ = id

instance Mutate UnaryOperator where
  mutExpr _ = id

instance Mutate Expr where
  mutExpr f = f

instance Mutate ConstExpr where
  mutExpr _ = id

instance Mutate Task where
  mutExpr f (Task i e) = Task i $ fmap f e

instance Mutate LVal where
  mutExpr f (RegExpr a e) = RegExpr a $ f e
  mutExpr _ a = a

instance Mutate PortDir where
  mutExpr _ = id

instance Mutate PortType where
  mutExpr _ = id

instance Mutate Range where
  mutExpr _ = id

instance Mutate Port where
  mutExpr _ = id

instance Mutate ModConn where
  mutExpr f (ModConn e) = ModConn $ f e
  mutExpr f (ModConnNamed a e) = ModConnNamed a $ f e

instance Mutate Assign where
  mutExpr f (Assign a b c) = Assign a b $ f c

instance Mutate ContAssign where
  mutExpr f (ContAssign a e) = ContAssign a $ f e

instance Mutate (CasePair ann) where
  mutExpr f (CasePair e s) = CasePair (f e) $ mutExpr f s

instance Mutate (Statement ann) where
  mutExpr f (TimeCtrl d s) = TimeCtrl d $ mutExpr f <$> s
  mutExpr f (EventCtrl e s) = EventCtrl e $ mutExpr f <$> s
  mutExpr f (SeqBlock s) = SeqBlock $ mutExpr f <$> s
  mutExpr f (BlockAssign a) = BlockAssign $ mutExpr f a
  mutExpr f (NonBlockAssign a) = NonBlockAssign $ mutExpr f a
  mutExpr f (TaskEnable a) = TaskEnable $ mutExpr f a
  mutExpr f (SysTaskEnable a) = SysTaskEnable $ mutExpr f a
  mutExpr f (CondStmnt a b c) = CondStmnt (f a) (mutExpr f <$> b) $ mutExpr f <$> c
  mutExpr f (ForLoop a1 e a2 s) = ForLoop a1 e a2 $ mutExpr f s
  mutExpr f (StmntAnn a s) = StmntAnn a $ mutExpr f s
  mutExpr f (StmntCase t e cp cd) = StmntCase t (f e) (mutExpr f cp) $ mutExpr f cd

instance Mutate Parameter where
  mutExpr _ = id

instance Mutate LocalParam where
  mutExpr _ = id

instance Mutate (ModItem ann) where
  mutExpr f (ModCA (ContAssign a e)) = ModCA . ContAssign a $ f e
  mutExpr f (ModInst a params b conns) = ModInst a (mutExpr f params) b $ mutExpr f conns
  mutExpr f (Initial s) = Initial $ mutExpr f s
  mutExpr f (Always s) = Always $ mutExpr f s
  mutExpr f (ModItemAnn a s) = ModItemAnn a $ mutExpr f s
  mutExpr _ d@Decl {} = d
  mutExpr _ p@ParamDecl {} = p
  mutExpr _ l@LocalParamDecl {} = l

instance Mutate (ModDecl ann) where
  mutExpr f (ModDecl a b c d e) =
    ModDecl (mutExpr f a) (mutExpr f b) (mutExpr f c) (mutExpr f d) (mutExpr f e)
  mutExpr f (ModDeclAnn a m) = ModDeclAnn a $ mutExpr f m

instance Mutate (Verilog ann) where
  mutExpr f (Verilog a) = Verilog $ mutExpr f a

instance Mutate (SourceInfo ann) where
  mutExpr f (SourceInfo a b) = SourceInfo a $ mutExpr f b

instance Mutate a => Mutate [a] where
  mutExpr f a = mutExpr f <$> a

instance Mutate a => Mutate (Maybe a) where
  mutExpr f a = mutExpr f <$> a

instance Mutate a => Mutate (GenVerilog a) where
  mutExpr f (GenVerilog a) = GenVerilog $ mutExpr f a

-- | Return if the 'Identifier' is in a '(ModDecl ann)'.
inPort :: Identifier -> (ModDecl ann) -> Bool
inPort i m = inInput
  where
    inInput =
      any (\a -> a ^. portName == i) $ m ^. modInPorts ++ m ^. modOutPorts

-- | Find the last assignment of a specific wire/reg to an expression, and
-- returns that expression.
findAssign :: Identifier -> [ModItem ann] -> Maybe Expr
findAssign i items = safe last . catMaybes $ isAssign <$> items
  where
    isAssign (ModCA (ContAssign val expr))
      | val == i = Just expr
      | otherwise = Nothing
    isAssign _ = Nothing

-- | Transforms an expression by replacing an Identifier with an
-- expression. This is used inside 'transformOf' and 'traverseExpr' to replace
-- the 'Identifier' recursively.
idTrans :: Identifier -> Expr -> Expr -> Expr
idTrans i expr (Id id')
  | id' == i = expr
  | otherwise = Id id'
idTrans _ _ e = e

-- | Replaces the identifier recursively in an expression.
replace :: Identifier -> Expr -> Expr -> Expr
replace = (transform .) . idTrans

-- | Nest expressions for a specific 'Identifier'. If the 'Identifier' is not
-- found, the AST is not changed.
--
-- This could be improved by instead of only using the last assignment to the
-- wire that one finds, to use the assignment to the wire before the current
-- expression. This would require a different approach though.
nestId :: Identifier -> (ModDecl ann) -> (ModDecl ann)
nestId i m
  | not $ inPort i m =
    let expr = fromMaybe def . findAssign i $ m ^. modItems
     in m & get %~ replace i expr
  | otherwise =
    m
  where
    get = modItems . traverse . modContAssign . contAssignExpr
    def = Id i

-- | Replaces an identifier by a expression in all the module declaration.
nestSource :: Identifier -> (Verilog ann) -> (Verilog ann)
nestSource i src = src & getModule %~ nestId i

-- | Nest variables in the format @w[0-9]*@ up to a certain number.
nestUpTo :: Int -> (Verilog ann) -> (Verilog ann)
nestUpTo i src =
  foldl (flip nestSource) src $ Identifier . fromNode <$> [1 .. i]

allVars :: (ModDecl ann) -> [Identifier]
allVars m =
  (m ^.. modOutPorts . traverse . portName)
    <> (m ^.. modInPorts . traverse . portName)

-- $setup
-- >>> import Verismith.Verilog.CodeGen
-- >>> let m = (ModDecl (Identifier "m") [Port Wire False 5 (Identifier "y")] [Port Wire False 5 "x"] [] [])
-- >>> let main = (ModDecl "main" [] [] [] [])

-- | Add a Module Instantiation using 'ModInst' from the first module passed to
-- it to the body of the second module. It first has to make all the inputs into
-- @reg@.
--
-- >>> render $ instantiateMod m main
-- module main;
--   wire [(3'h4):(1'h0)] y;
--   reg [(3'h4):(1'h0)] x;
--   m m1(y, x);
-- endmodule
-- <BLANKLINE>
-- <BLANKLINE>
instantiateMod :: (ModDecl ann) -> (ModDecl ann) -> (ModDecl ann)
instantiateMod m main = main & modItems %~ ((out ++ regIn ++ [inst]) ++)
  where
    out = Decl Nothing <$> m ^. modOutPorts <*> pure Nothing
    regIn =
      Decl Nothing
        <$> (m ^. modInPorts & traverse . portType .~ Reg)
        <*> pure Nothing
    inst =
      ModInst
        (m ^. modId) []
        (m ^. modId <> (Identifier . showT $ count + 1))
        conns
    count =
      length
        . filter (== m ^. modId)
        $ main
          ^.. modItems
          . traverse
          . modInstId
    conns = uncurry ModConnNamed . fmap Id <$> zip (allVars m) (allVars m)

-- | Instantiate without adding wire declarations. It also does not count the
-- current instantiations of the same module.
--
-- >>> GenVerilog $ instantiateMod_ m
-- m m(y, x);
-- <BLANKLINE>
instantiateMod_ :: (ModDecl ann) -> (ModItem ann)
instantiateMod_ m = ModInst (m ^. modId) [] (m ^. modId) conns
  where
    conns =
      ModConn
        . Id
        <$> (m ^.. modOutPorts . traverse . portName)
        ++ (m ^.. modInPorts . traverse . portName)

-- | Instantiate without adding wire declarations. It also does not count the
-- current instantiations of the same module.
--
-- >>> GenVerilog $ instantiateModSpec_ "_" m
-- m m(.y(y), .x(x));
-- <BLANKLINE>
instantiateModSpec_ :: Bool -> Text -> (ModDecl ann) -> (ModItem ann)
instantiateModSpec_ named outChar m = ModInst (m ^. modId) [] (m ^. modId) conns
  where
    conns = (if named then zipWith ModConnNamed ids else map ModConn) (Id <$> instIds)
    ids = filterChar outChar (name modOutPorts) <> name modInPorts
    instIds = name modOutPorts <> name modInPorts
    name v = m ^.. v . traverse . portName

filterChar :: Text -> [Identifier] -> [Identifier]
filterChar t ids =
  ids & traverse . _Wrapped %~ (\x -> fromMaybe x . safe head $ T.splitOn t x)

-- | Initialise all the inputs and outputs to a module.
--
-- >>> GenVerilog $ initMod m
-- module m(y, x);
--   output wire [(3'h4):(1'h0)] y;
--   input wire [(3'h4):(1'h0)] x;
-- endmodule
-- <BLANKLINE>
-- <BLANKLINE>
initMod :: (ModDecl ann) -> (ModDecl ann)
initMod m = m & modItems %~ ((out ++ inp) ++)
  where
    out = Decl (Just PortOut) <$> (m ^. modOutPorts) <*> pure Nothing
    inp = Decl (Just PortIn) <$> (m ^. modInPorts) <*> pure Nothing

-- | Make an 'Identifier' from and existing Identifier and an object with a
-- 'Show' instance to make it unique.
makeIdFrom :: (Show a) => a -> Identifier -> Identifier
makeIdFrom a i = (i <>) . Identifier . ("_" <>) $ showT a

-- | Make top level module for equivalence verification. Also takes in how many
-- modules to instantiate.
makeTop :: Bool -> Int -> (ModDecl ann) -> (ModDecl ann)
makeTop named i m = ModDecl (m ^. modId) ys (m ^. modInPorts) modIt []
  where
    ys = yPort . flip makeIdFrom "y" <$> [1 .. i]
    modIt = instantiateModSpec_ named "_" . modN <$> [1 .. i]
    modN n =
      m & modId %~ makeIdFrom n & modOutPorts .~ [yPort (makeIdFrom n "y")]

-- | Make a top module with an assert that requires @y_1@ to always be equal to
-- @y_2@, which can then be proven using a formal verification tool.
makeTopAssert :: (ModDecl ann) -> (ModDecl ann)
makeTopAssert = (modItems %~ (++ [assert])) . makeTop False 2
  where
    assert =
      Always . EventCtrl e . Just $
        SeqBlock
          [TaskEnable $ Task "assert" [BinOp (Id "y_1") BinEq (Id "y_2")]]
    e = EPosEdge "clk"

-- | Provide declarations for all the ports that are passed to it. If they are
-- registers, it should assign them to 0.
declareMod :: [Port] -> (ModDecl ann) -> (ModDecl ann)
declareMod ports = initMod . (modItems %~ (fmap decl ports ++))
  where
    decl p@(Port Reg _ _ _) = Decl Nothing p (Just 0)
    decl p = Decl Nothing p Nothing

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
simplify (BinOp (Number (BitVec _ 1)) BinAnd e) = e
simplify (BinOp e BinAnd (Number (BitVec _ 1))) = e
simplify (BinOp (Number (BitVec _ 0)) BinAnd _) = Number 0
simplify (BinOp _ BinAnd (Number (BitVec _ 0))) = Number 0
simplify (BinOp e BinPlus (Number (BitVec _ 0))) = e
simplify (BinOp (Number (BitVec _ 0)) BinPlus e) = e
simplify (BinOp e BinMinus (Number (BitVec _ 0))) = e
simplify (BinOp (Number (BitVec _ 0)) BinMinus e) = e
simplify (BinOp e BinTimes (Number (BitVec _ 1))) = e
simplify (BinOp (Number (BitVec _ 1)) BinTimes e) = e
simplify (BinOp _ BinTimes (Number (BitVec _ 0))) = Number 0
simplify (BinOp (Number (BitVec _ 0)) BinTimes _) = Number 0
simplify (BinOp e BinOr (Number (BitVec _ 0))) = e
simplify (BinOp (Number (BitVec _ 0)) BinOr e) = e
simplify (BinOp e BinLSL (Number (BitVec _ 0))) = e
simplify (BinOp (Number (BitVec _ 0)) BinLSL e) = e
simplify (BinOp e BinLSR (Number (BitVec _ 0))) = e
simplify (BinOp (Number (BitVec _ 0)) BinLSR e) = e
simplify (BinOp e BinASL (Number (BitVec _ 0))) = e
simplify (BinOp (Number (BitVec _ 0)) BinASL e) = e
simplify (BinOp e BinASR (Number (BitVec _ 0))) = e
simplify (BinOp (Number (BitVec _ 0)) BinASR e) = e
simplify (UnOp UnPlus e) = e
simplify e = e

-- | Remove all 'Identifier' that do not appeare in the input list from an
-- 'Expr'. The identifier will be replaced by @1'b0@, which can then later be
-- simplified further.
--
-- >>> GenVerilog . removeId ["x"] $ Id "x" + Id "y"
-- (x + (1'h0))
removeId :: [Identifier] -> Expr -> Expr
removeId i = transform trans
  where
    trans (Id ident)
      | ident `notElem` i = Number 0
      | otherwise = Id ident
    trans e = e

combineAssigns :: Port -> [ModItem ann] -> [ModItem ann]
combineAssigns p a =
  a
    <> [ ModCA
           . ContAssign (p ^. portName)
           . UnOp UnXor
           . fold
           $ Id
             <$> assigns
       ]
  where
    assigns = a ^.. traverse . modContAssign . contAssignNetLVal

combineAssigns_ :: Bool -> Port -> [Port] -> (ModItem ann)
combineAssigns_ comb p ps =
  ModCA
    . ContAssign (p ^. portName)
    . (if comb then UnOp UnXor else id)
    . fold
    $ Id
      <$> ps
      ^.. traverse
      . portName

fromPort :: Port -> Identifier
fromPort (Port _ _ _ i) = i
