module Main where

import           Control.Lens
import qualified Crypto.Random.DRBG                as C
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString                   as B
import qualified Data.Graph.Inductive              as G
import qualified Data.Graph.Inductive.Arbitrary    as G
import qualified Data.Graph.Inductive.Dot          as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import qualified Data.Text.IO                      as T
import qualified Data.Text.Lazy                    as T
import           Numeric                           (showHex)
import           Numeric.Natural                   (Natural)
import           Shelly
import qualified Test.QuickCheck                   as QC
import           VeriFuzz
import qualified VeriFuzz.Graph.RandomAlt          as V

genRand :: C.CtrDRBG -> Int -> [ByteString] -> [ByteString]
genRand gen n bytes
  | n == 0 = ranBytes : bytes
  | otherwise = genRand newGen (n-1) $ ranBytes : bytes
  where
    Right (ranBytes, newGen) = C.genBytes 32 gen

genRandom :: Int -> IO [ByteString]
genRandom n = do
  gen <- C.newGenIO :: IO C.CtrDRBG
  return $ genRand gen n []

runSimulation :: IO ()
runSimulation = do
  gr <- QC.generate $ rDups <$> QC.resize 100 (randomDAG :: QC.Gen (G.Gr Gate ()))
  -- let dot = G.showDot . G.fglToDotString $ G.nemap show (const "") gr
  -- writeFile "file.dot" dot
  -- shelly $ run_ "dot" ["-Tpng", "-o", "file.png", "file.dot"]
  let circ = head $ (nestUpTo 5 . generateAST $ Circuit gr) ^.. getVerilogSrc . traverse . getDescription
  rand <- genRandom 20
  val <- shelly $ runSim defaultIcarus (initMod circ) rand
  putStrLn $ showHex val ""

runEquivalence:: IO ()
runEquivalence = do
  gr <- QC.generate $ rDups <$> QC.resize 100 (randomDAG :: QC.Gen (G.Gr Gate ()))
  let circ = initMod . head $ (nestUpTo 5 . generateAST $ Circuit gr) ^.. getVerilogSrc . traverse . getDescription
  shelly . verbosely $ runEquiv defaultYosys defaultYosys (Just defaultXst) circ

main :: IO ()
 --main = sample (arbitrary :: Gen (Circuit Input))
main =
  -- runEquivalence
  runSimulation
