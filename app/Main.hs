module Main where

import           Control.Concurrent
import           Control.Lens
import qualified Crypto.Random.DRBG   as C
import           Data.ByteString      (ByteString)
import qualified Data.Graph.Inductive as G
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Numeric              (showHex)
import           Prelude              hiding (FilePath)
import           Shelly
import qualified Test.QuickCheck      as QC
import           VeriFuzz

myForkIO :: IO () -> IO (MVar ())
myForkIO io = do
  mvar <- newEmptyMVar
  _ <- forkFinally io (\_ -> putMVar mvar ())
  return mvar

genRand :: C.CtrDRBG -> Int -> [ByteString] -> [ByteString]
genRand gen n bytes | n == 0    = ranBytes : bytes
                    | otherwise = genRand newGen (n - 1) $ ranBytes : bytes
  where Right (ranBytes, newGen) = C.genBytes 32 gen

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
  let circ =
        head $ (nestUpTo 5 . generateAST $ Circuit gr) ^.. getVerilogSrc . traverse . getDescription
  rand <- genRandom 20
  val  <- shelly $ runSim defaultIcarus (initMod circ) rand
  putStrLn $ showHex (abs val) ""

onFailure :: Text -> RunFailed -> Sh ()
onFailure t _ = do
  cd ".."
  cp_r (fromText t) $ fromText (t <> "_failed")

runEquivalence :: Text -> Int -> IO ()
runEquivalence t i = do
  gr <- QC.generate $ rDups <$> QC.resize 1000 (randomDAG :: QC.Gen (G.Gr Gate ()))
  let circ =
        initMod
          .   head
          $   (nestUpTo 5 . generateAST $ Circuit gr)
          ^.. getVerilogSrc
          .   traverse
          .   getDescription
  shellyFailDir . verbosely $ do
    mkdir_p (fromText "equiv" </> fromText n)
    curr <- toTextIgnore <$> pwd
    setenv "VERIFUZZ_ROOT" curr
    cd (fromText "equiv" </> fromText n)
    catch_sh (runEquiv defaultYosys defaultYosys (Just defaultXst) circ) $ onFailure n
    cd ".."
  --runEquivalence t $ i+1
  where
    n = t <> "_" <> T.pack (show i)

main :: IO ()
 --main = sample (arbitrary :: Gen (Circuit Input))
main = do
  num <- getNumCapabilities
  vars <- sequence $ (\x -> myForkIO $
                       runEquivalence ("test_" <> T.pack (show x)) 0) <$> [1..num]
  sequence_ $ takeMVar <$> vars
