module Main where

import qualified Data.Graph.Inductive              as G
import qualified Data.Graph.Inductive.Arbitrary    as G
import qualified Data.Graph.Inductive.Dot          as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import qualified Data.Text.IO                      as T
import qualified Data.Text.Lazy                    as T
import           Shelly
import qualified Test.QuickCheck                   as QC
import           Test.VeriFuzz
import qualified Test.VeriFuzz.Graph.RandomAlt     as V

main :: IO ()
 --main = sample (arbitrary :: Gen (Circuit Input))
main = do
  gr <- QC.generate $ rDups <$> QC.resize 30 (randomDAG :: QC.Gen (G.Gr Gate ()))
  let dot = G.showDot . G.fglToDotString $ G.nemap show (const "") gr
  writeFile "file.dot" dot
  shelly $ run_ "dot" ["-Tpng", "-o", "file.png", "file.dot"]
  -- T.putStrLn $ generate gr
  -- g <- QC.generate (QC.resize 5 (QC.arbitrary :: QC.Gen VerilogSrc))
  render . genVerilogSrc . addTestBench . nestUpTo 5 . generateAST $ Circuit gr
  -- render . genVerilogSrc . addTestBench $ g
