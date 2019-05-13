{-|
Module      : VeriFuzz
Description : VeriFuzz
Copyright   : (c) 2018-2019, Yann Herklotz
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX
-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module VeriFuzz
    ( defaultMain
    , Opts(..)
    , runEquivalence
    , runSimulation
    , runReduce
    , draw
    , SourceInfo(..)
    , module VeriFuzz.Verilog
    , module VeriFuzz.Config
    , module VeriFuzz.Circuit
    , module VeriFuzz.Sim
    , module VeriFuzz.Fuzz
    , module VeriFuzz.Report
    )
where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad.IO.Class   (liftIO)
import qualified Crypto.Random.DRBG       as C
import           Data.ByteString          (ByteString)
import           Data.ByteString.Builder  (byteStringHex, toLazyByteString)
import qualified Data.ByteString.Lazy     as L
import qualified Data.Graph.Inductive     as G
import qualified Data.Graph.Inductive.Dot as G
import           Data.Maybe               (isNothing)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8)
import qualified Data.Text.IO             as T
import           Hedgehog                 (Gen)
import qualified Hedgehog.Gen             as Hog
import           Hedgehog.Internal.Seed   (Seed)
import           Options.Applicative
import           Prelude                  hiding (FilePath)
import           Shelly                   hiding (command)
import           Shelly.Lifted            (liftSh)
import           System.Random            (randomIO)
import           VeriFuzz.Circuit
import           VeriFuzz.Config
import           VeriFuzz.Fuzz
import           VeriFuzz.Reduce
import           VeriFuzz.Report
import           VeriFuzz.Result
import           VeriFuzz.Sim
import           VeriFuzz.Sim.Internal
import           VeriFuzz.Verilog

data OptTool = TYosys
          | TXST
          | TIcarus

instance Show OptTool where
  show TYosys  = "yosys"
  show TXST    = "xst"
  show TIcarus = "icarus"

data Opts = Fuzz { fuzzOutput :: {-# UNPACK #-} !Text
                 , configFile :: !(Maybe FilePath)
                 , forced     :: !Bool
                 , keepAll    :: !Bool
                 , num        :: {-# UNPACK #-} !Int
                 }
          | Generate { mFileName  :: !(Maybe FilePath)
                     , configFile :: !(Maybe FilePath)
                     }
          | Parse { fileName :: {-# UNPACK #-} !FilePath
                  }
          | Reduce { fileName     :: {-# UNPACK #-} !FilePath
                   , top          :: {-# UNPACK #-} !Text
                   , reduceScript :: {-# UNPACK #-} !FilePath
                   }
          | ConfigOpt { writeConfig :: !(Maybe FilePath)
                      , configFile  :: !(Maybe FilePath)
                      , doRandomise :: !Bool
                      }

myForkIO :: IO () -> IO (MVar ())
myForkIO io = do
    mvar <- newEmptyMVar
    _    <- forkFinally io (\_ -> putMVar mvar ())
    return mvar

textOption :: Mod OptionFields String -> Parser Text
textOption = fmap T.pack . strOption

optReader :: (String -> Maybe a) -> ReadM a
optReader f = eitherReader $ \arg -> case f arg of
    Just a  -> Right a
    Nothing -> Left $ "Cannot parse option: " <> arg

parseSynth :: String -> Maybe OptTool
parseSynth val | val == "yosys" = Just TYosys
               | val == "xst"   = Just TXST
               | otherwise      = Nothing

parseSim :: String -> Maybe OptTool
parseSim val | val == "icarus" = Just TIcarus
             | otherwise       = Nothing

fuzzOpts :: Parser Opts
fuzzOpts =
    Fuzz
        <$> textOption
                (  long "output"
                <> short 'o'
                <> metavar "DIR"
                <> help "Output directory that the fuzz run takes place in."
                <> showDefault
                <> value "output"
                )
        <*> (  optional
            .  strOption
            $  long "config"
            <> short 'c'
            <> metavar "FILE"
            <> help "Config file for the current fuzz run."
            )
        <*> (switch $ long "force" <> short 'f' <> help
                "Overwrite the specified directory."
            )
        <*> (switch $ long "keep" <> short 'k' <> help
                "Keep all the directories."
            )
        <*> (  option auto
            $  long "num"
            <> short 'n'
            <> help "The number of fuzz runs that should be performed."
            <> showDefault
            <> value 1
            <> metavar "INT"
            )

genOpts :: Parser Opts
genOpts =
    Generate
        <$> (  optional
            .  strOption
            $  long "output"
            <> short 'o'
            <> metavar "FILE"
            <> help "Output to a verilog file instead."
            )
        <*> (  optional
            .  strOption
            $  long "config"
            <> short 'c'
            <> metavar "FILE"
            <> help "Config file for the generation run."
            )

parseOpts :: Parser Opts
parseOpts = Parse . fromText . T.pack <$> strArgument
    (metavar "FILE" <> help "Verilog input file.")

reduceOpts :: Parser Opts
reduceOpts =
    Reduce
        .   fromText
        .   T.pack
        <$> strArgument (metavar "FILE" <> help "Verilog input file.")
        <*> textOption
                (  short 't'
                <> long "top"
                <> metavar "TOP"
                <> help "Name of top level module."
                <> showDefault
                <> value "top"
                )
        <*> (strOption
            $  long "script"
            <> short 's'
            <> metavar "SCRIPT"
            <> help "Script that determines if the current file is interesting, which is determined by the script returning 0."
            )

configOpts :: Parser Opts
configOpts =
    ConfigOpt
        <$> (  optional
            .  strOption
            $  long "output"
            <> short 'o'
            <> metavar "FILE"
            <> help "Output to a TOML Config file."
            )
        <*> (  optional
            .  strOption
            $  long "config"
            <> short 'c'
            <> metavar "FILE"
            <> help "Config file for the current fuzz run."
            )
        <*> (switch $ long "randomise" <> short 'r' <> help
                "Randomise the given default config, or the default config by randomly switchin on and off options."
            )

argparse :: Parser Opts
argparse =
    hsubparser
            (  command
                    "fuzz"
                    (info
                        fuzzOpts
                        (progDesc
                            "Run fuzzing on the specified simulators and synthesisers."
                        )
                    )
            <> metavar "fuzz"
            )
        <|> hsubparser
                (  command
                        "generate"
                        (info
                            genOpts
                            (progDesc "Generate a random Verilog program.")
                        )
                <> metavar "generate"
                )
        <|> hsubparser
                (  command
                        "parse"
                        (info
                            parseOpts
                            (progDesc
                                "Parse a verilog file and output a pretty printed version."
                            )
                        )
                <> metavar "parse"
                )
        <|> hsubparser
                (  command
                        "reduce"
                        (info
                            reduceOpts
                            (progDesc
                                "Reduce a Verilog file by rerunning the fuzzer on the file."
                            )
                        )
                <> metavar "reduce"
                )
        <|> hsubparser
                (  command
                        "config"
                        (info
                            configOpts
                            (progDesc
                                "Print the current configuration of the fuzzer."
                            )
                        )
                <> metavar "config"
                )

version :: Parser (a -> a)
version = infoOption versionInfo $ mconcat
    [long "version", short 'v', help "Show version information.", hidden]

opts :: ParserInfo Opts
opts = info
    (argparse <**> helper <**> version)
    (  fullDesc
    <> progDesc "Fuzz different simulators and synthesisers."
    <> header
           "VeriFuzz - A hardware simulator and synthesiser Verilog fuzzer."
    )

getConfig :: Maybe FilePath -> IO Config
getConfig s = maybe (return defaultConfig) parseConfigFile $ T.unpack . toTextIgnore <$> s

-- | Randomly remove an option by setting it to 0.
randDelete :: Int -> IO Int
randDelete i = do
    r <- randomIO
    return $ if r then i else 0

randomise :: Config -> IO Config
randomise config@(Config a _ c d e) = do
    mia <- randDelete $ cm ^. probModItemAssign
    misa <- return $ cm ^. probModItemSeqAlways
    mica <- randDelete $ cm ^. probModItemCombAlways
    mii <- randDelete $ cm ^. probModItemInst
    ssb <- randDelete $ cs ^. probStmntBlock
    ssnb <- randDelete $ cs ^. probStmntNonBlock
    ssc <- randDelete $ cs ^. probStmntCond
    ssf <- randDelete $ cs ^. probStmntFor
    en <- return $ ce ^. probExprNum
    ei <- randDelete $ ce ^. probExprId
    ers <- randDelete $ ce ^. probExprRangeSelect
    euo <- randDelete $ ce ^. probExprUnOp
    ebo <- randDelete $ ce ^. probExprBinOp
    ec <- randDelete $ ce ^. probExprCond
    eco <- randDelete $ ce ^. probExprConcat
    estr <- randDelete $ ce ^. probExprStr
    esgn <- randDelete $ ce ^. probExprSigned
    eus <- randDelete $ ce ^. probExprUnsigned
    return $ Config a (Probability
                       (ProbModItem mia misa mica mii)
                       (ProbStatement ssb ssnb ssc ssf)
                       (ProbExpr en ei ers euo ebo ec eco estr esgn eus)) c d e
    where
        cm = config ^. configProbability . probModItem
        cs = config ^. configProbability . probStmnt
        ce = config ^. configProbability . probExpr

handleOpts :: Opts -> IO ()
handleOpts (Fuzz _ configF _ _ n) = do
    config <- getConfig configF
    _ <- runFuzz config
                   defaultYosys
                   (fuzzMultiple n Nothing (proceduralSrc "top" config))
    return ()
handleOpts (Generate f c) = do
    config <- getConfig c
    source <- proceduralIO "top" config
    maybe (T.putStrLn $ genSource source)
          (flip T.writeFile $ genSource source)
          $ T.unpack . toTextIgnore <$> f
handleOpts (Parse f) = do
    verilogSrc <- T.readFile file
    case parseVerilog (T.pack file) verilogSrc of
        Left  l -> print l
        Right v -> print $ GenVerilog v
    where file = T.unpack . toTextIgnore $ f
handleOpts (Reduce f t s) = shelly $ reduceWithScript t s f
handleOpts (ConfigOpt c conf r) = do
    config <- if r then getConfig conf >>= randomise else getConfig conf
    maybe (T.putStrLn . encodeConfig $ config)
        (`encodeConfigFile` config)
        $ T.unpack . toTextIgnore <$> c

defaultMain :: IO ()
defaultMain = do
    optsparsed <- execParser opts
    handleOpts optsparsed

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
    val   <- shelly . runResultT $ runSim defaultIcarus (makeSrcInfo rand2) rand
    case val of
        Pass a -> T.putStrLn $ showBS a
        _      -> T.putStrLn "Test failed"


-- | Code to be executed on a failure. Also checks if the failure was a timeout,
-- as the timeout command will return the 124 error code if that was the
-- case. In that case, the error will be moved to a different directory.
onFailure :: Text -> RunFailed -> Sh (Result Failed ())
onFailure t _ = do
    ex <- lastExitCode
    case ex of
        124 -> do
            logger "Test TIMEOUT"
            chdir ".." $ cp_r (fromText t) $ fromText (t <> "_timeout")
            return $ Fail EmptyFail
        _ -> do
            logger "Test FAIL"
            chdir ".." $ cp_r (fromText t) $ fromText (t <> "_failed")
            return $ Fail EmptyFail

checkEquivalence :: SourceInfo -> Text -> IO Bool
checkEquivalence src dir = shellyFailDir $ do
    mkdir_p (fromText dir)
    curr <- toTextIgnore <$> pwd
    setenv "VERIFUZZ_ROOT" curr
    cd (fromText dir)
    catch_sh
        (  (runResultT $ runEquiv defaultYosys (Just defaultVivado) src)
        >> return True
        )
        ((\_ -> return False) :: RunFailed -> Sh Bool)

-- | Run a fuzz run and check if all of the simulators passed by checking if the
-- generated Verilog files are equivalent.
runEquivalence
    :: Maybe Seed
    -> Gen Verilog -- ^ Generator for the Verilog file.
    -> Text        -- ^ Name of the folder on each thread.
    -> Text        -- ^ Name of the general folder being used.
    -> Bool        -- ^ Keep flag.
    -> Int         -- ^ Used to track the recursion.
    -> IO ()
runEquivalence seed gm t d k i = do
    (_, m) <- shelly $ sampleSeed seed gm
    let srcInfo = SourceInfo "top" m
    rand <- generateByteString 20
    shellyFailDir $ do
        mkdir_p (fromText d </> fromText n)
        curr <- toTextIgnore <$> pwd
        setenv "VERIFUZZ_ROOT" curr
        cd (fromText "output" </> fromText n)
        _ <-
            catch_sh
                    (  runResultT
                    $  runEquiv defaultYosys (Just defaultVivado) srcInfo
                    >> liftSh (logger "Test OK")
                    )
                $ onFailure n
        _ <-
            catch_sh
                    (   runResultT
                    $   runSim (Icarus "iverilog" "vvp") srcInfo rand
                    >>= (\b -> liftSh $ logger ("RTL Sim: " <> showBS b))
                    )
                $ onFailure n
        cd ".."
        unless k . rm_rf $ fromText n
    when (i < 5 && isNothing seed) (runEquivalence seed gm t d k $ i + 1)
    where n = t <> "_" <> T.pack (show i)

runReduce :: SourceInfo -> IO SourceInfo
runReduce s = shelly $ reduce (\s' -> not <$> liftIO (checkEquivalence s' "reduce")) s
