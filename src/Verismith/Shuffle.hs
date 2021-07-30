{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      : Verismith.Shuffle
-- Description : Shuffle Verilog around.
-- Copyright   : (c) 2021, Yann Herklotz
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- Shuffles the Verilog file around a bit.

module Verismith.Shuffle where

import Control.Lens hiding (Context)
import Control.Monad (replicateM)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Text (Text)
import Data.List (intercalate, partition)
import Data.Containers.ListUtils (nubOrd)
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
import qualified Data.Map.Strict as Map

import Verismith.Verilog.CodeGen
import Verismith.Verilog.Quote
import qualified Data.Text.IO as T

import Hedgehog (Gen, GenT, MonadGen)

-- | Shuffles assign statements and always blocks in a Verilog file.
shuffleLinesModule :: (MonadGen m) => ModDecl a -> m (ModDecl a)
shuffleLinesModule m = do
  shuf' <- Hog.shuffle shuf
  return (m&modItems.~(stat <> shuf'))
  where
    (shuf, stat) = partition (\x -> case x of
                                   Always _ -> True
                                   ModCA _ -> True
                                   _ -> False
                               ) (m^.modItems)

renameIdent :: Map.Map Text Text -> Identifier -> Identifier
renameIdent map (Identifier e) = Identifier $ Map.findWithDefault e e map

renameExpr :: Map.Map Text Text -> Expr -> Expr
renameExpr map (Id e) = Id (renameIdent map e)
renameExpr map (VecSelect e a) = VecSelect (renameIdent map e) a
renameExpr map (RangeSelect e r) = RangeSelect (renameIdent map e) r
renameExpr map (Appl e r) = Appl (renameIdent map e) r
renameExpr _ e = e

renameVariablesModule :: (MonadGen m) => ModDecl a -> m (ModDecl a)
renameVariablesModule m = do
  shuf' <- Hog.shuffle ids
  let map = Map.fromList $ zip ids shuf'
  return (m &
          (modItems.traverse %~ (mutExpr (transform $ renameExpr map)))
        . (modOutPorts.traverse.portName %~ renameIdent map)
        . (modInPorts.traverse.portName %~ renameIdent map)
        . (modItems.traverse.declPort.portName %~ renameIdent map)
        . (transformOn (modItems.traverse._Always) (stmntBA.assignReg.regId %~ renameIdent map))
        . (transformOn (modItems.traverse._Always) (stmntNBA.assignReg.regId %~ renameIdent map))
        . (transformOn (modItems.traverse._Initial) (stmntBA.assignReg.regId %~ renameIdent map))
        . (transformOn (modItems.traverse._Initial) (stmntNBA.assignReg.regId %~ renameIdent map))
        . (modItems.traverse.modContAssign.contAssignNetLVal %~ renameIdent map))
  where
    ids = nubOrd $ (concatMap universe $ allExprCA <> allExprStmnt)^..traverse._Id._Wrapped
    allExprCA = m^..modItems.traverse.modContAssign.contAssignExpr
    allExprStmnt =
      (allStat^..traverse.stmntCondExpr)
      <> (allStat^..traverse.stmntCaseExpr)
      <> (allStat^..traverse.forExpr)
      <> (allStat^..traverse.stmntBA.assignExpr)
      <> (allStat^..traverse.stmntNBA.assignExpr)
    allStat = concatMap universe stat
    stat = (m^..modItems.traverse._Initial)
           <> (m^..modItems.traverse._Always)

identModule :: (MonadGen m) => ModDecl a -> m (ModDecl a)
identModule = return

applyModules :: (MonadGen m) => (ModDecl a -> m (ModDecl a)) -> SourceInfo a -> m (SourceInfo a)
applyModules f s = do
  ms' <- sequence (f <$> ms)
  return (s & infoSrc._Wrapped .~ ms')
  where
    ms = s^.infoSrc._Wrapped

shuffleLines, renameVariables, identityMod :: (SourceInfo a) -> Gen (SourceInfo a)
shuffleLines    = applyModules shuffleLinesModule
renameVariables = applyModules renameVariablesModule
identityMod     = applyModules identModule

shuffleLinesIO :: (SourceInfo a) -> IO (SourceInfo a)
shuffleLinesIO = Hog.sample . shuffleLines

renameVariablesIO :: (SourceInfo a) -> IO (SourceInfo a)
renameVariablesIO =  Hog.sample . renameVariables

m' :: SourceInfo ()
m' = SourceInfo "m" [verilog|
module fir_kernel_4tap_arch_1 #(
    parameter   BW = 32,
    parameter   SW = 5
) (X1, X2, X3, X5, X4, S, result);
    input  [BW-1:0] X1;
    input  [BW-1:0] X2;
    input  [BW-1:0] X3;
    input  [BW-1:0] X5;
    input  [BW-1:0] X4;
    input  [SW-1:0] S;
    output [BW-1:0] result;
    reg [SW:0] two_shift;
    wire [BW-1:0] first_sum;
    wire [BW-1:0] second_sum;

    always @* two_shift = S<<1;
    assign first_sum = X1 ? X1[2:0] : X1;
    assign second_sum = X4 + (X5 << S);
    assign result = ((first_sum >> two_shift) + second_sum) >> S;
endmodule
|]

renameExample, shuffleExample :: IO (GenVerilog (SourceInfo ()))
renameExample = GenVerilog <$> Hog.sample (renameVariables m')
shuffleExample = GenVerilog <$> Hog.sample (shuffleLines m')
