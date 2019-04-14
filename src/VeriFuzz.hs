{-|
Module      : VeriFuzz
Description : VeriFuzz
Copyright   : (c) 2018-2019, Yann Herklotz
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX
-}

module VeriFuzz
    ( runEquivalence
    , runSimulation
    , runReduce
    , draw
    , SourceInfo(..)
    , module VeriFuzz.Verilog
    , module VeriFuzz.Config
    , module VeriFuzz.Circuit
    , module VeriFuzz.Sim
    )
where

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
import           Hedgehog                 (Gen)
import qualified Hedgehog.Gen             as Hog
import           Prelude                  hiding (FilePath)
import           Shelly
import           VeriFuzz.Circuit
import           VeriFuzz.Config
import           VeriFuzz.Sim
import           VeriFuzz.Verilog

-- | Generate a specific number of random bytestrings of size 256.
randomByteString :: C.CtrDRBG -> Int -> [ByteString] -> [ByteString]
randomByteString gen n bytes
    | n == 0    = ranBytes : bytes
    | otherwise = randomByteString newGen (n - 1) $ ranBytes : bytes
    where Right (ranBytes, newGen) = C.genBytes 32 gen

-- | generates the specific number of bytestring with a random seed.
generateByteString :: Int -> IO [ByteString]
generateByteString n = do
    gen <- C.newGenIO :: IO C.CtrDRBG
    return $ randomByteString gen n []

makeSrcInfo :: ModDecl -> SourceInfo
makeSrcInfo m = SourceInfo (getIdentifier $ m ^. modId) (Verilog [m])

-- | Draw a randomly generated DAG to a dot file and compile it to a png so it
-- can be seen.
draw :: IO ()
draw = do
    gr <- Hog.sample $ rDups . getCircuit <$> Hog.resize 10 randomDAG
    let dot = G.showDot . G.fglToDotString $ G.nemap show (const "") gr
    writeFile "file.dot" dot
    shelly $ run_ "dot" ["-Tpng", "-o", "file.png", "file.dot"]

-- | Function to show a bytestring in a hex format.
showBS :: ByteString -> Text
showBS = decodeUtf8 . L.toStrict . toLazyByteString . byteStringHex

-- | Run a simulation on a random DAG or a random module.
runSimulation :: IO ()
runSimulation = do
  -- gr <- Hog.generate $ rDups <$> Hog.resize 100 (randomDAG :: Gen (G.Gr Gate ()))
  -- let dot = G.showDot . G.fglToDotString $ G.nemap show (const "") gr
  -- writeFile "file.dot" dot
  -- shelly $ run_ "dot" ["-Tpng", "-o", "file.png", "file.dot"]
  -- let circ =
  --       head $ (nestUpTo 30 . generateAST $ Circuit gr) ^.. getVerilog . traverse . getDescription
    rand  <- generateByteString 20
    rand2 <- Hog.sample (randomMod 10 100)
    val   <- shelly $ runSim defaultIcarus (makeSrcInfo rand2) rand
    T.putStrLn $ showBS val


-- | Code to be executed on a failure. Also checks if the failure was a timeout,
-- as the timeout command will return the 124 error code if that was the
-- case. In that case, the error will be moved to a different directory.
onFailure :: Text -> RunFailed -> Sh ()
onFailure t _ = do
    ex <- lastExitCode
    case ex of
        124 -> do
            echoP "Test TIMEOUT"
            chdir ".." $ cp_r (fromText t) $ fromText (t <> "_timeout")
        _ -> do
            echoP "Test FAIL"
            chdir ".." $ cp_r (fromText t) $ fromText (t <> "_failed")

checkEquivalence :: SourceInfo -> Text -> IO Bool
checkEquivalence src dir = shellyFailDir $ do
    mkdir_p (fromText dir)
    curr <- toTextIgnore <$> pwd
    setenv "VERIFUZZ_ROOT" curr
    cd (fromText dir)
    catch_sh
        (  runEquiv defaultYosys defaultYosys (Just defaultVivado) src
        >> return True
        )
        ((\_ -> return False) :: RunFailed -> Sh Bool)

-- | Run a fuzz run and check if all of the simulators passed by checking if the
-- generated Verilog files are equivalent.
runEquivalence
    :: Gen Verilog -- ^ Generator for the Verilog file.
    -> Text        -- ^ Name of the folder on each thread.
    -> Text        -- ^ Name of the general folder being used.
    -> Bool        -- ^ Keep flag.
    -> Int         -- ^ Used to track the recursion.
    -> IO ()
runEquivalence gm t d k i = do
    m <- Hog.sample gm
    let srcInfo = SourceInfo "top" m
    rand <- generateByteString 20
    shellyFailDir $ do
        mkdir_p (fromText d </> fromText n)
        curr <- toTextIgnore <$> pwd
        setenv "VERIFUZZ_ROOT" curr
        cd (fromText "output" </> fromText n)
        catch_sh
                (runEquiv defaultYosys defaultYosys (Just defaultVivado) srcInfo
                >> echoP "Test OK"
                )
            $ onFailure n
        catch_sh
                (   runSim (Icarus "iverilog" "vvp") srcInfo rand
                >>= (\b -> echoP ("RTL Sim: " <> showBS b))
                )
            $ onFailure n
        cd ".."
        unless k . rm_rf $ fromText n
    when (i < 5) (runEquivalence gm t d k $ i + 1)
    where n = t <> "_" <> T.pack (show i)

runReduce :: SourceInfo -> IO SourceInfo
runReduce = reduce $ flip checkEquivalence "reduce"
