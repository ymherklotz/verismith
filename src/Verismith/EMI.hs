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
  ( genEMI,
  )
where

import Control.Lens hiding (Context)
import Control.Monad (replicateM)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog (Gen, GenT, MonadGen)
import qualified Hedgehog as Hog
import qualified Hedgehog.Gen as Hog
import qualified Hedgehog.Range as Hog
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

newPort' :: Identifier -> StateGen a Port
newPort' ident = do
  let p = Port Wire False (Range 1 1) ident
  emiContext . _Just . emiNewInputs %= (p :)
  return p

statementEMI :: Statement a -> StateGen a (Statement a)
statementEMI (SeqBlock s) = do
  s' <- statement
  n <- newPort' "x"
  let s'' = CondStmnt (Id "x") (Just s') Nothing
  return $ SeqBlock (s'' : s)
statementEMI (EventCtrl e (Just s)) = EventCtrl e . Just <$> (statementEMI s)
statementEMI s = return s

moditemEMI :: ModItem a -> StateGen a (ModItem a)
moditemEMI (Always s) = Always <$> statementEMI s
moditemEMI m = return m

genEMI :: (ModDecl a) -> StateGen a (ModDecl a)
genEMI (ModDecl mid outp inp itms params) = do
  itms' <- traverse moditemEMI itms
  return (ModDecl mid outp inp itms' params)

-- | Procedural generation method for random Verilog. Uses internal 'Reader' and
-- 'State' to keep track of the current Verilog code structure.
proceduralEMI :: ModDecl a -> Config -> Gen (ModDecl a)
proceduralEMI moddecl config = do
  (mainMod, st) <-
    Hog.resize num $
      runStateT
        (Hog.distributeT (runReaderT (genEMI moddecl) config))
        context
  let mainMod' = mainMod & modInPorts %~ (++ (st ^. emiContext . _Just . emiNewInputs))
  return mainMod'
  where
    context =
      Context [] [] [] [] [] [] 0 (confProp propStmntDepth) (confProp propModDepth) True
        (Just (EMIContext []))
    num = fromIntegral $ confProp propSize
    confProp i = config ^. configProperty . i

proceduralEMIIO :: ModDecl () -> Config -> IO (ModDecl ())
proceduralEMIIO t = Hog.sample . proceduralEMI t
