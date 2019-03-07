module Main where

import           Control.Concurrent
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Options.Applicative
import qualified Shelly              as S
import qualified Test.QuickCheck     as QC
import qualified VeriFuzz            as V

data Tool = Yosys
          | XST
          | Icarus

instance Show Tool where
  show Yosys  = "yosys"
  show XST    = "xst"
  show Icarus = "icarus"

data Opts = Fuzz { fuzzOutput :: {-# UNPACK #-} !Text
                 }
          | Rerun { tools :: [Tool]
                  , input :: {-# UNPACK #-} !S.FilePath
                  }
          | Generate { fileName :: {-# UNPACK #-} !S.FilePath
                     }
          | Parse { fileName :: {-# UNPACK #-} !S.FilePath
                  }
          | Reduce { fileName :: {-# UNPACK #-} !S.FilePath
                   , top      :: {-# UNPACK #-} !Text
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

parseSynth :: String -> Maybe Tool
parseSynth val | val == "yosys" = Just Yosys
               | val == "xst"   = Just XST
               | otherwise      = Nothing

parseSim :: String -> Maybe Tool
parseSim val | val == "icarus" = Just Icarus
             | otherwise       = Nothing

fuzzOpts :: Parser Opts
fuzzOpts = Fuzz <$> textOption
    (  long "output"
    <> short 'o'
    <> metavar "DIR"
    <> help "Output directory that the fuzz run takes place in."
    <> showDefault
    <> value "output"
    )

rerunOpts :: Parser Opts
rerunOpts =
    Rerun
        <$> some (option
                    (optReader parseSynth)
                    (  long "synth"
                    <> metavar "SYNTH"
                    <> help "Rerun using a synthesiser (yosys|xst)."
                    <> showDefault
                    <> value Yosys
                    )
            <|> option
                    (optReader parseSim)
                    (  long "sim"
                    <> metavar "SIM"
                    <> help "Rerun using a simulator (icarus)."
                    <> showDefault
                    <> value Icarus
                    )
            )
        <*> (S.fromText <$> textOption
            ( long "input"
              <> short 'i'
              <> metavar "FILE"
              <> help "Verilog file input."
              <> showDefault
              <> value "rtl.v"))

genOpts :: Parser Opts
genOpts = Generate . S.fromText <$> textOption
    (  long "output"
    <> short 'o'
    <> metavar "FILE"
    <> help "Verilog output file."
    <> showDefault
    <> value "main.v"
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
                        "rerun"
                        (info
                            rerunOpts
                            (progDesc
                                "Rerun a Verilog file with a simulator or a synthesiser."
                            )
                        )
                <> metavar "rerun"
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

opts :: ParserInfo Opts
opts = info
    (argparse <**> helper)
    (  fullDesc
    <> progDesc "Fuzz different simulators and synthesisers."
    <> header
           "VeriFuzz - A hardware simulator and synthesiser Verilog fuzzer."
    )

handleOpts :: Opts -> IO ()
handleOpts (Fuzz _) = do
    num  <- getNumCapabilities
    vars <-
        sequence
        $   (\x -> myForkIO $ V.runEquivalence (V.randomMod 10 100)
                                               ("test_" <> T.pack (show x))
                                               0
            )
        <$> [1 .. num]
    sequence_ $ takeMVar <$> vars
handleOpts (Generate f) = do
    g <- QC.generate $ V.randomMod 50 1000
    S.shelly . S.writefile f $ V.genSource g
handleOpts (Parse f) = do
    verilogSrc <- readFile file
    case V.parseVerilog file verilogSrc of
        Left  l -> print l
        Right v -> print $ V.GenVerilog v
    where file = T.unpack . S.toTextIgnore $ f
handleOpts (Rerun _ _) = undefined
handleOpts (Reduce f t) = do
    verilogSrc <- readFile file
    case V.parseVerilog file verilogSrc of
        Left  l -> print l
        Right v -> do
            writeFile "main.v" . T.unpack $ V.genSource (V.SourceInfo t v)
            vreduced <- V.runReduce (V.SourceInfo t v)
            writeFile "reduced.v" . T.unpack $ V.genSource vreduced
    where file = T.unpack $ S.toTextIgnore f

main :: IO ()
main = do
    optsparsed <- execParser opts
    handleOpts optsparsed
