{-|
Module      : Simulation
Description : Simulation module for Main.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Simulation module for Main.
-}

module Simulation where

import           Control.Lens
import qualified Crypto.Random.DRBG       as C
import           Data.ByteString          (ByteString)
import           Data.ByteString.Builder  (byteStringHex, toLazyByteString)
import qualified Data.ByteString.Lazy     as L
import qualified Data.Graph.Inductive     as G
import qualified Data.Graph.Inductive.Dot as G
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8)
import qualified Data.Text.IO             as T
import           Prelude                  hiding (FilePath)
import           Shelly
import           Test.QuickCheck          (Gen)
import qualified Test.QuickCheck          as QC
import           VeriFuzz

genRand :: C.CtrDRBG -> Int -> [ByteString] -> [ByteString]
genRand gen n bytes | n == 0    = ranBytes : bytes
                    | otherwise = genRand newGen (n - 1) $ ranBytes : bytes
  where Right (ranBytes, newGen) = C.genBytes 32 gen

genRandom :: Int -> IO [ByteString]
genRandom n = do
  gen <- C.newGenIO :: IO C.CtrDRBG
  return $ genRand gen n []

draw :: IO ()
draw = do
  gr <- QC.generate $ rDups <$> QC.resize 10 (randomDAG :: QC.Gen (G.Gr Gate ()))
  let dot = G.showDot . G.fglToDotString $ G.nemap show (const "") gr
  writeFile "file.dot" dot
  shelly $ run_ "dot" ["-Tpng", "-o", "file.png", "file.dot"]

runSimulation :: IO ()
runSimulation = do
  gr <- QC.generate $ rDups <$> QC.resize 100 (randomDAG :: QC.Gen (G.Gr Gate ()))
  -- let dot = G.showDot . G.fglToDotString $ G.nemap show (const "") gr
  -- writeFile "file.dot" dot
  -- shelly $ run_ "dot" ["-Tpng", "-o", "file.png", "file.dot"]
  let circ =
        head $ (nestUpTo 30 . generateAST $ Circuit gr) ^.. getVerilogSrc . traverse . getDescription
  rand <- genRandom 20
  rand2 <- QC.generate (randomMod 10 100)
  val  <- shelly $ runSim defaultIcarus (rand2) rand
  T.putStrLn . decodeUtf8 $ (L.toStrict . toLazyByteString . byteStringHex $ val)

onFailure :: Text -> RunFailed -> Sh ()
onFailure t _ = do
  ex <- lastExitCode
  case ex of
    124 -> do
      echoP "Test TIMEOUT"
      cd ".."
      cp_r (fromText t) $ fromText (t <> "_timeout")
    _ -> do
      echoP "Test FAIL"
      cd ".."
      cp_r (fromText t) $ fromText (t <> "_failed")

runEquivalence :: Gen ModDecl -> Text -> Int -> IO ()
runEquivalence gm t i = do
  m <- QC.generate gm
  shellyFailDir $ do
    mkdir_p (fromText "output" </> fromText n)
    curr <- toTextIgnore <$> pwd
    setenv "VERIFUZZ_ROOT" curr
    cd (fromText "output" </> fromText n)
    catch_sh (runEquiv defaultYosys defaultYosys
              (Just defaultXst) m >> echoP "Test OK" >> cd "..") $
      onFailure n
    rm_rf $ fromText n
  when (i < 5) (runEquivalence gm t $ i+1)
  where
    n = t <> "_" <> T.pack (show i)
