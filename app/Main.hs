module Main where

import qualified Data.Graph.Inductive              as G
import qualified Data.Graph.Inductive.Arbitrary    as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import qualified Data.GraphViz                     as Gviz
import qualified Data.GraphViz.Attributes.Complete as Gviz
import qualified Data.Text.IO                      as T
import qualified Data.Text.Lazy                    as T
import qualified Test.QuickCheck                   as QC
import           Test.VeriFuzz
import qualified Test.VeriFuzz.Graph.RandomAlt     as V

instance Gviz.Labellable Gate where
  toLabelValue gate = Gviz.StrLabel . T.pack $ show gate

main :: IO ()
 --main = sample (arbitrary :: Gen (Circuit Input))
main = do
  gr <- QC.generate $ rDups <$> QC.resize 15 (randomDAG :: QC.Gen (G.Gr Gate ()))
  let dot = Gviz.graphToDot Gviz.nonClusteredParams . G.emap (const "") $ gr
  _ <- Gviz.runGraphviz dot Gviz.Png "output.png"
  return ()
  -- T.putStrLn $ generate gr
  -- g <- QC.generate (QC.resize 5 (QC.arbitrary :: QC.Gen VerilogSrc))
  render . genVerilogSrc . addTestBench . nestUpTo 5 . generateAST $ Circuit gr

  -- render . genVerilogSrc . addTestBench $ g
