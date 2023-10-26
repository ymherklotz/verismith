{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      : Verismith.EMI
-- Description : Definition of the circuit graph.
-- Copyright   : (c) 2021, Yann Herklotz
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- Equivalence modulo inputs (EMI) testing.  This file should get an existing design, and spit out a
-- modified design that is equivalent under some specific values of the extra inputs.
module Verismith.EMI where

import Control.Lens hiding (Context)
import Control.Monad (replicateM)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Hedgehog (Gen, GenT, MonadGen)
import qualified Hedgehog as Hog
import qualified Hedgehog.Gen as Hog
import qualified Hedgehog.Range as HogR
import Verismith.Config
import Verismith.Generate
import Verismith.Utils
import Verismith.Verilog.AST
import Verismith.Verilog.BitVec
import Verismith.Verilog.CodeGen
import Verismith.Verilog.Eval
import Verismith.Verilog.Internal
import Verismith.Verilog.Mutate
import Verismith.Verilog.Quote

data EMIInputs a
  = EMIInputs [Identifier]
  | EMIOrig a
  deriving (Eq, Ord)

instance (Show a) => Show (EMIInputs a) where
  show (EMIInputs i) = "EMI: " <> intercalate ", " (T.unpack . getIdentifier <$> i)
  show (EMIOrig a) = show a

newPort' :: Identifier -> StateGen a Port
newPort' ident = do
  hex <- Identifier . T.toLower . T.pack <$> Hog.list (HogR.constant 10 10) Hog.hexit
  let p = Port Wire False (Range 0 0) (ident <> hex)
  emiContext . _Just . emiNewInputs %= (p :)
  return p

nstatementEMI :: StateGen a (Maybe (Statement a))
nstatementEMI = do
  config <- ask
  Hog.frequency
    [ ( config ^. configEMI . confEMIGenerateProb,
        do
          s' <- statement
          n <- newPort' "emi_"
          return (Just (CondStmnt (Id (n ^. portName)) (Just s') Nothing))
      ),
      (config ^. configEMI . confEMINoGenerateProb, return Nothing)
    ]

statementEMI :: Statement a -> StateGen a (Statement a)
statementEMI (SeqBlock s) = do
  s'' <- nstatementEMI
  return $ SeqBlock ((s'' ^.. _Just) ++ s)
statementEMI s = return s

moditemEMI :: ModItem a -> StateGen a (ModItem a)
moditemEMI (Always s) = Always <$> transformM statementEMI s
moditemEMI m = return m

moddeclEMI :: ModDecl a -> StateGen a (ModDecl (EMIInputs a))
moddeclEMI m = do
  emiContext . _Just . emiNewInputs .= []
  blocking .= []
  nonblocking .= []
  wires .= []
  m' <- traverseOf (modItems . traverse) moditemEMI m
  c <- use (emiContext . _Just . emiNewInputs)
  b <- use blocking
  nb <- use nonblocking
  w <- use wires
  let m'' = m' & modInPorts %~ (c ++) & initNewRegs c & initNewInnerRegs (b <> nb <> w)
  return (ModDeclAnn (EMIInputs (c ^.. traverse . portName)) (fmap (\x -> EMIOrig x) m''))

sourceEMI :: (SourceInfo a) -> StateGen a (SourceInfo (EMIInputs a))
sourceEMI s =
  traverseOf (infoSrc . _Wrapped . traverse) moddeclEMI s

initNewRegs :: [Port] -> ModDecl a -> ModDecl a
initNewRegs ps m = m & modItems %~ (++ (Decl (Just PortIn) <$> ps <*> pure Nothing))

initNewInnerRegs :: [Port] -> ModDecl a -> ModDecl a
initNewInnerRegs ps m = m & modItems %~ (++ (Decl Nothing <$> ps <*> pure Nothing))

-- | Procedural generation method for random Verilog. Uses internal 'Reader' and
-- 'State' to keep track of the current Verilog code structure.
proceduralEMI :: SourceInfo a -> Config -> Gen (SourceInfo (EMIInputs a))
proceduralEMI src config = do
  (mainMod, st) <-
    Hog.resize num $
      runStateT
        (Hog.distributeT (runReaderT (sourceEMI src) config))
        context
  return mainMod
  where
    context =
      Context
        []
        []
        []
        []
        []
        []
        100000
        (confProp propStmntDepth)
        (confProp propModDepth)
        True
        (Just (EMIContext []))
    num = fromIntegral $ confProp propSize
    confProp i = config ^. configProperty . i

proceduralEMIIO :: SourceInfo a -> Config -> IO (SourceInfo (EMIInputs a))
proceduralEMIIO t = Hog.sample . proceduralEMI t

-- | Make top level module for equivalence verification. Also takes in how many
-- modules to instantiate.
makeTopEMI :: Int -> ModDecl (EMIInputs ann) -> (ModDecl (EMIInputs ann), [Identifier])
makeTopEMI i m' = (ModDecl (m ^. modId) ys nports modIt [], anns)
  where
    ys = yPort . flip makeIdFrom "y" <$> [1 .. i]
    modIt = instantiateModSpec_ True "_" . modN <$> [1 .. i]
    modN n =
      m & modId %~ makeIdFrom n & modOutPorts .~ [yPort (makeIdFrom n "y")]
    anns =
      concatMap
        ( \x -> case x of
            EMIInputs x -> x
            _ -> []
        )
        (collectAnn m')
    m = removeAnn m'
    nports = filter (\x -> (x ^. portName) `notElem` anns) (m ^. modInPorts)

createProperty :: Identifier -> ModItem a
createProperty i =
  Property (i <> "_emi_prop") (EPosEdge "clk") Nothing (BinOp (Id i) BinEq 0)

createAssignment :: Identifier -> Statement a
createAssignment i = BlockAssign (Assign (RegId i) Nothing 0)

addAssumesEMI ::
  (ModDecl a, [Identifier]) ->
  (ModDecl a, [Identifier])
addAssumesEMI (m, i) = (m & modItems %~ (++ mods), i)
  where
    mods = fmap createProperty i

addAssignmentsEMI ::
  (ModDecl a, [Identifier]) ->
  (ModDecl a, [Identifier])
addAssignmentsEMI (m, i) = (m & modItems %~ (mods :), i)
  where
    mods = Initial (SeqBlock (createAssignment <$> i))

-- | Make a top module with an assert that requires @y_1@ to always be equal to
-- @y_2@, which can then be proven using a formal verification tool.
makeTopAssertEMI :: Bool -> ModDecl (EMIInputs ann) -> (ModDecl (EMIInputs ann), [Identifier])
makeTopAssertEMI b =
  bimap (modItems %~ (assert :)) id
    . (if b then addAssumesEMI else addAssignmentsEMI)
    . makeTopEMI 2
  where
    assert =
      Always . EventCtrl e . Just $
        SeqBlock
          [TaskEnable $ Task "assert" [BinOp (Id "y_1") BinEq (Id "y_2")]]
    e = EPosEdge "clk"

initModEMI :: (ModDecl ann, [Identifier]) -> (ModDecl ann)
initModEMI (m, i) = m & modItems %~ ((out ++ inp ++ other) ++)
  where
    out = Decl (Just PortOut) <$> (m ^. modOutPorts) <*> pure Nothing
    inp = Decl (Just PortIn) <$> (m ^. modInPorts) <*> pure Nothing
    other = Decl Nothing <$> map (\i' -> Port Reg False (Range 0 0) i') i <*> pure Nothing

getTopEMIIdent :: SourceInfo (EMIInputs a) -> [Identifier]
getTopEMIIdent s =
  concatMap
    ( \x -> case x of
        EMIInputs x -> x
        _ -> []
    )
    (collectAnn (s ^. mainModule))

-- Test code

m :: SourceInfo ()
m =
  SourceInfo
    "m"
    [verilog|
module m;
  always @(posedge clk) begin
    if (z == 2) begin
      ry = 2;
    end
    x <= y;
    y <= z;
  end
endmodule

module m2;
  always @(posedge clk) begin
    if (z == 2) begin
      ry = 2;
    end
    x <= y;
    y <= z;
  end
endmodule
|]

p :: (Show a) => ModDecl a -> IO ()
p = T.putStrLn . genSource

p2 :: (Show a) => SourceInfo a -> IO ()
p2 = T.putStrLn . genSource

customConfig =
  defaultConfig
    & (configEMI . confEMIGenerateProb .~ 1)
      . (configEMI . confEMINoGenerateProb .~ 0)

top = ((initModEMI . makeTopAssertEMI True . (\s -> s ^. mainModule)) <$> proceduralEMIIO m customConfig) >>= p

top2 = proceduralEMIIO m customConfig >>= p2
