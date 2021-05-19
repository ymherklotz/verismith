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
module Verismith.EMI
where

import Control.Lens hiding (Context)
import Control.Monad (replicateM)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Text (Text)
import Data.List (intercalate)
import qualified Data.Text as T
import Hedgehog (Gen, GenT, MonadGen)
import qualified Hedgehog as Hog
import qualified Hedgehog.Gen as Hog
import qualified Hedgehog.Range as HogR
import Data.Maybe (fromMaybe)
import Verismith.Config
import Verismith.Internal
import Verismith.Verilog.AST
import Verismith.Verilog.BitVec
import Verismith.Verilog.Eval
import Verismith.Verilog.Internal
import Verismith.Verilog.Mutate
import Verismith.Generate

import Verismith.Verilog.CodeGen
import Verismith.Verilog.Quote
import qualified Data.Text.IO as T

data EMIInputs a = EMIInputs [Identifier]
                 | EMIOrig a
                 deriving (Eq)

instance Show a => Show (EMIInputs a) where
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
    [ (config ^. configEMI . confEMIGenerateProb, do
          s' <- statement
          n <- newPort' "emi_"
          return (Just (CondStmnt (Id (n^.portName)) (Just s') Nothing))),
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
  emiContext._Just.emiNewInputs .= []
  m' <- traverseOf (modItems.traverse) moditemEMI m
  c <- use (emiContext._Just.emiNewInputs)
  return (ModDeclAnn (EMIInputs (c^..traverse.portName)) (fmap (\x -> EMIOrig x) m'))

sourceEMI :: (SourceInfo a) -> StateGen a (SourceInfo (EMIInputs a))
sourceEMI s =
  traverseOf (infoSrc._Wrapped.traverse) moddeclEMI s

initNewRegs :: [Port] -> ModDecl a -> ModDecl a
initNewRegs ps m = m & modItems %~ (++ (Decl (Just PortIn) <$> ps <*> pure Nothing))

-- | Procedural generation method for random Verilog. Uses internal 'Reader' and
-- 'State' to keep track of the current Verilog code structure.
proceduralEMI :: SourceInfo a -> Config -> Gen (SourceInfo (EMIInputs a))
proceduralEMI src config = do
  (mainMod, st) <-
    Hog.resize num $
      runStateT
        (Hog.distributeT (runReaderT (sourceEMI src) config))
        context
  let addMod = modInPorts %~ ((st ^. emiContext._Just.emiNewInputs) ++ )
  let initMod = initNewRegs (st ^. emiContext._Just.emiNewInputs)
  return (mainMod & infoSrc._Wrapped.traverse %~ (initMod . addMod))
  where
    context =
      Context [] [] [] [] [] [] 0 (confProp propStmntDepth) (confProp propModDepth) True
        (Just (EMIContext []))
    num = fromIntegral $ confProp propSize
    confProp i = config ^. configProperty . i

proceduralEMIIO :: SourceInfo a -> Config -> IO (SourceInfo (EMIInputs a))
proceduralEMIIO t = Hog.sample . proceduralEMI t

-- Test code

m = SourceInfo "m" [verilog|
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
p :: Show a => SourceInfo a -> IO ()
p = T.putStrLn . genSource

customConfig = defaultConfig &
    (configEMI . confEMIGenerateProb .~ 1)
  . (configEMI . confEMINoGenerateProb .~ 0)
