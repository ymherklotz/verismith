module Main where

import           Control.Concurrent
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Options.Applicative
import qualified Shelly              as S
import qualified VeriFuzz            as V

data Tool = Yosys
          | XST
          | Icarus

instance Show Tool where
  show Yosys  = "yosys"
  show XST    = "xst"
  show Icarus = "icarus"

data Opts = Fuzz { fuzzOutput :: {-# UNPACK #-} !Text
                 , configFile :: !(Maybe FilePath)
                 , forced     :: !Bool
                 , keepAll    :: !Bool
                 , num        :: {-# UNPACK #-} !Int
                 }
          | Generate { mFileName  :: !(Maybe FilePath)
                     , configFile :: !(Maybe FilePath)
                     }
          | Parse { fileName :: {-# UNPACK #-} !S.FilePath
                  }
          | Reduce { fileName :: {-# UNPACK #-} !S.FilePath
                   , top      :: {-# UNPACK #-} !Text
                   }
          | Config { writeDefaultConfig :: !(Maybe FilePath) }

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

parseSynth :: String -> Maybe Tool
parseSynth val | val == "yosys" = Just Yosys
               | val == "xst"   = Just XST
               | otherwise      = Nothing

parseSim :: String -> Maybe Tool
parseSim val | val == "icarus" = Just Icarus
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
        <*> (option auto $
             long "num"
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
parseOpts = Parse . S.fromText . T.pack <$> strArgument
    (metavar "FILE" <> help "Verilog input file.")

reduceOpts :: Parser Opts
reduceOpts =
    Reduce
        .   S.fromText
        .   T.pack
        <$> strArgument (metavar "FILE" <> help "Verilog input file.")
        <*> textOption
                (  short 't'
                <> long "top"
                <> metavar "TOP"
                <> help "Name of top level module."
                <> showDefault
                <> value "main"
                )

configOpts :: Parser Opts
configOpts =
    Config
        <$> (  optional
            .  strOption
            $  long "output"
            <> short 'o'
            <> metavar "FILE"
            <> help "Output to a TOML Config file."
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

opts :: ParserInfo Opts
opts = info
    (argparse <**> helper)
    (  fullDesc
    <> progDesc "Fuzz different simulators and synthesisers."
    <> header
           "VeriFuzz - A hardware simulator and synthesiser Verilog fuzzer."
    )

getConfig :: Maybe FilePath -> IO V.Config
getConfig = maybe (return V.defaultConfig) V.parseConfigFile

handleOpts :: Opts -> IO ()
handleOpts (Fuzz out configF _ _ n) = do
    config <- getConfig configF
    _ <- V.runFuzz config
                   V.defaultYosys
                   (V.fuzzMultiple n Nothing (V.proceduralSrc "top" config))
    return ()
handleOpts (Generate f c) = do
    config <- getConfig c
    source <- V.proceduralIO "top" config
    maybe (T.putStrLn $ V.genSource source)
          (flip T.writeFile (V.genSource source))
          f
handleOpts (Parse f) = do
    verilogSrc <- readFile file
    case V.parseVerilog file verilogSrc of
        Left  l -> print l
        Right v -> print $ V.GenVerilog v
    where file = T.unpack . S.toTextIgnore $ f
handleOpts (Reduce f t) = do
    verilogSrc <- readFile file
    case V.parseVerilog file verilogSrc of
        Left  l -> print l
        Right v -> do
            writeFile "main.v" . T.unpack $ V.genSource (V.SourceInfo t v)
            vreduced <- V.runReduce (V.SourceInfo t v)
            writeFile "reduced.v" . T.unpack $ V.genSource vreduced
    where file = T.unpack $ S.toTextIgnore f
handleOpts (Config c) = maybe
    (T.putStrLn . V.encodeConfig $ V.defaultConfig)
    (`V.encodeConfigFile` V.defaultConfig)
    c

main :: IO ()
main = do
    optsparsed <- execParser opts
    handleOpts optsparsed
