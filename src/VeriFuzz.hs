{-|
Module      : VeriFuzz
Description : VeriFuzz
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX
-}

module VeriFuzz
    ( runEquivalence
    , runSimulation
    , draw
    , module VeriFuzz.AST
    , module VeriFuzz.ASTGen
    , module VeriFuzz.Circuit
    , module VeriFuzz.CodeGen
    , module VeriFuzz.Env
    , module VeriFuzz.Gen
    , module VeriFuzz.General
    , module VeriFuzz.Icarus
    , module VeriFuzz.Internal
    , module VeriFuzz.Mutate
    , module VeriFuzz.Parser
    , module VeriFuzz.Random
    , module VeriFuzz.XST
    , module VeriFuzz.Yosys
    )
where

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
import           VeriFuzz.AST
import           VeriFuzz.ASTGen
import           VeriFuzz.Circuit
import           VeriFuzz.CodeGen
import           VeriFuzz.Env
import           VeriFuzz.Gen
import           VeriFuzz.General
import           VeriFuzz.Icarus
import           VeriFuzz.Internal
import           VeriFuzz.Mutate
import           VeriFuzz.Parser
import           VeriFuzz.Random
import           VeriFuzz.XST
import           VeriFuzz.Yosys

-- | Generate a specific number of random bytestrings of size 256.
genRand :: C.CtrDRBG -> Int -> [ByteString] -> [ByteString]
genRand gen n bytes | n == 0    = ranBytes : bytes
                    | otherwise = genRand newGen (n - 1) $ ranBytes : bytes
    where Right (ranBytes, newGen) = C.genBytes 32 gen

-- | generates the specific number of bytestring with a random seed.
genRandom :: Int -> IO [ByteString]
genRandom n = do
    gen <- C.newGenIO :: IO C.CtrDRBG
    return $ genRand gen n []

-- | Draw a randomly generated DAG to a dot file and compile it to a png so it
-- can be seen.
draw :: IO ()
draw = do
    gr <- QC.generate $ rDups <$> QC.resize
        10
        (randomDAG :: QC.Gen (G.Gr Gate ()))
    let dot = G.showDot . G.fglToDotString $ G.nemap show (const "") gr
    writeFile "file.dot" dot
    shelly $ run_ "dot" ["-Tpng", "-o", "file.png", "file.dot"]

-- | Function to show a bytestring in a hex format.
showBS :: ByteString -> Text
showBS = decodeUtf8 . L.toStrict . toLazyByteString . byteStringHex

-- | Run a simulation on a random DAG or a random module.
runSimulation :: IO ()
runSimulation = do
  -- gr <- QC.generate $ rDups <$> QC.resize 100 (randomDAG :: QC.Gen (G.Gr Gate ()))
  -- let dot = G.showDot . G.fglToDotString $ G.nemap show (const "") gr
  -- writeFile "file.dot" dot
  -- shelly $ run_ "dot" ["-Tpng", "-o", "file.png", "file.dot"]
  -- let circ =
  --       head $ (nestUpTo 30 . generateAST $ Circuit gr) ^.. getVerilogSrc . traverse . getDescription
    rand  <- genRandom 20
    rand2 <- QC.generate (randomMod 10 100)
    val   <- shelly $ runSim defaultIcarus rand2 rand
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

-- | Run a fuzz run and check if all of the simulators passed by checking if the
-- generated Verilog files are equivalent.
runEquivalence :: Gen ModDecl -> Text -> Int -> IO ()
runEquivalence gm t i = do
    m    <- QC.generate gm
    rand <- genRandom 20
    shellyFailDir $ do
        mkdir_p (fromText "output" </> fromText n)
        curr <- toTextIgnore <$> pwd
        setenv "VERIFUZZ_ROOT" curr
        cd (fromText "output" </> fromText n)
        catch_sh
                (  runEquiv defaultYosys defaultYosys (Just defaultXst) m
                >> echoP "Test OK"
                )
            $ onFailure n
        catch_sh
                (   runSim (Icarus "iverilog" "vvp") m rand
                >>= (\b -> echoP ("RTL Sim: " <> showBS b))
                )
            $ onFailure n
    --    catch_sh (runSimWithFile (Icarus "iverilog" "vvp") "syn_yosys.v" rand
    --              >>= (\b -> echoP ("Yosys Sim: " <> showBS b))) $
    --      onFailure n
    --    catch_sh (runSimWithFile (Icarus "iverilog" "vvp") "syn_xst.v" rand
    --              >>= (\b -> echoP ("XST Sim: " <> showBS b))) $
    --      onFailure n
        cd ".."
        rm_rf $ fromText n
    when (i < 5) (runEquivalence gm t $ i + 1)
    where n = t <> "_" <> T.pack (show i)
