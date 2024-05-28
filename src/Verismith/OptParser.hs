module Verismith.OptParser
  ( OptTool (..),
    Opts (..),
    opts,
  )
where

import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
  ( Mod (..),
    OptionFields (..),
    Parser (..),
    ParserInfo (..),
    ReadM (..),
    (<**>),
  )
import qualified Options.Applicative as Opt
import Shelly (FilePath (..), fromText)
import Verismith.Config (SynthDescription (..), versionInfo)
import Prelude hiding (FilePath (..))

data OptTool
  = TYosys
  | TXST
  | TIcarus

instance Show OptTool where
  show TYosys = "yosys"
  show TXST = "xst"
  show TIcarus = "icarus"

data Opts
  = Fuzz
      { fuzzOutput :: Text,
        fuzzConfigFile :: !(Maybe FilePath),
        fuzzForced :: !Bool,
        fuzzKeepAll :: !Bool,
        fuzzNum :: {-# UNPACK #-} !Int,
        fuzzNoSim :: !Bool,
        fuzzNoEquiv :: !Bool,
        fuzzNoReduction :: !Bool,
        fuzzExistingFile :: !(Maybe FilePath),
        fuzzExistingFileTop :: !Text,
        fuzzCrossCheck :: !Bool,
        fuzzChecker :: !(Maybe Text)
      }
  | EMIOpts
      { emiOutput :: Text,
        emiConfigFile :: !(Maybe FilePath),
        emiForced :: !Bool,
        emiKeepAll :: !Bool,
        emiNum :: {-# UNPACK #-} !Int,
        emiNoSim :: !Bool,
        emiNoEquiv :: !Bool,
        emiNoReduction :: !Bool,
        emiTopModule :: Text,
        emiInputFile :: FilePath
      }
  | Generate
      { generateFilename :: !(Maybe FilePath),
        generateConfigFile :: !(Maybe FilePath),
        generateValidSyntax :: !Bool
      }
  | Parse
      { parseFilename :: !FilePath,
        parseOutput :: !(Maybe FilePath),
        parseStrict :: !Bool
      }
  | Reduce
      { reduceFilename :: !FilePath,
        reduceTop :: !Text,
        reduceScript :: !(Maybe FilePath),
        reduceSynthesiserDesc :: ![SynthDescription],
        reduceRerun :: !Bool
      }
  | ConfigOpt
      { configOptWriteConfig :: !(Maybe FilePath),
        configOptConfigFile :: !(Maybe FilePath),
        configOptDoRandomise :: !Bool
      }
  | DistanceOpt
      { distanceOptVerilogA :: !FilePath,
        distanceOptVerilogB :: !FilePath
      }
  | ShuffleOpt
      { shuffleOptFilename :: !FilePath,
        shuffleOptTop :: !Text,
        shuffleOptOutput :: !(Maybe FilePath),
        shuffleOptShuffleLines :: !Bool,
        shuffleOptRenameVars :: !Bool,
        shuffleOptEquiv :: !Bool,
        shuffleOptEquivFolder :: !FilePath,
        shuffleOptChecker :: !(Maybe Text)
      }
  | Equiv
      { equivOutput :: !FilePath,
        equivFilenameA :: !FilePath,
        equivFilenameB :: !FilePath,
        equivFileTop :: !Text,
        equivChecker :: !(Maybe Text)
      }

textOption :: Mod OptionFields String -> Parser Text
textOption = fmap T.pack . Opt.strOption

optReader :: (String -> Maybe a) -> ReadM a
optReader f = Opt.eitherReader $ \arg -> case f arg of
  Just a -> Right a
  Nothing -> Left $ "Cannot parse option: " <> arg

parseSynth :: String -> Maybe OptTool
parseSynth val
  | val == "yosys" = Just TYosys
  | val == "xst" = Just TXST
  | otherwise = Nothing

parseSynthDesc :: String -> Maybe SynthDescription
parseSynthDesc val
  | val == "yosys" = Just $ SynthDescription "yosys" Nothing Nothing Nothing
  | val == "vivado" = Just $ SynthDescription "vivado" Nothing Nothing Nothing
  | val == "xst" = Just $ SynthDescription "xst" Nothing Nothing Nothing
  | val == "quartus" =
    Just $
      SynthDescription "quartus" Nothing Nothing Nothing
  | val == "identity" =
    Just $
      SynthDescription "identity" Nothing Nothing Nothing
  | otherwise = Nothing

parseSim :: String -> Maybe OptTool
parseSim val
  | val == "icarus" = Just TIcarus
  | otherwise = Nothing

fuzzOpts :: Parser Opts
fuzzOpts =
  Fuzz
    <$> textOption
      ( Opt.long "output"
          <> Opt.short 'o'
          <> Opt.metavar "DIR"
          <> Opt.help "Output directory that the fuzz run takes place in."
          <> Opt.showDefault
          <> Opt.value "output"
      )
    <*> ( Opt.optional
            . Opt.strOption
            $ Opt.long "config"
              <> Opt.short 'c'
              <> Opt.metavar "FILE"
              <> Opt.help "Config file for the current fuzz run."
        )
    <*> ( Opt.switch $
            Opt.long "force"
              <> Opt.short 'f'
              <> Opt.help
                "Overwrite the specified directory."
        )
    <*> ( Opt.switch $
            Opt.long "keep"
              <> Opt.short 'k'
              <> Opt.help
                "Keep all the directories."
        )
    <*> ( Opt.option Opt.auto $
            Opt.long "num"
              <> Opt.short 'n'
              <> Opt.help "The number of fuzz runs that should be performed."
              <> Opt.showDefault
              <> Opt.value 1
              <> Opt.metavar "INT"
        )
    <*> ( Opt.switch $
            Opt.long "no-sim"
              <> Opt.help
                "Do not run simulation on the output netlist."
        )
    <*> ( Opt.switch $
            Opt.long "no-equiv"
              <> Opt.help
                "Do not run an equivalence check on the output netlist."
        )
    <*> ( Opt.switch $
            Opt.long "no-reduction"
              <> Opt.help
                "Do not run reduction on a failed testcase."
        )
    <*> ( Opt.optional
            . Opt.strOption
            $ Opt.long "source"
              <> Opt.short 's'
              <> Opt.metavar "FILE"
              <> Opt.help "Name of the top module."
        )
    <*> textOption
      ( Opt.long "source-top"
          <> Opt.short 't'
          <> Opt.metavar "TOP"
          <> Opt.help "Define the top module for the source file."
          <> Opt.showDefault
          <> Opt.value "top"
      )
    <*> ( Opt.switch $
            Opt.long "crosscheck"
              <> Opt.help
                "Do not only compare against the original design, but also against other netlists."
        )
    <*> ( Opt.optional . textOption $
            Opt.long "checker"
              <> Opt.metavar "CHECKER"
              <> Opt.help "Define the checker to use."
        )

emiOpts :: Parser Opts
emiOpts =
  EMIOpts
    <$> textOption
      ( Opt.long "output"
          <> Opt.short 'o'
          <> Opt.metavar "DIR"
          <> Opt.help "Output directory that the fuzz run takes place in."
          <> Opt.showDefault
          <> Opt.value "output"
      )
    <*> ( Opt.optional
            . Opt.strOption
            $ Opt.long "config"
              <> Opt.short 'c'
              <> Opt.metavar "FILE"
              <> Opt.help "Config file for the current fuzz run."
        )
    <*> ( Opt.switch $
            Opt.long "force"
              <> Opt.short 'f'
              <> Opt.help
                "Overwrite the specified directory."
        )
    <*> ( Opt.switch $
            Opt.long "keep"
              <> Opt.short 'k'
              <> Opt.help
                "Keep all the directories."
        )
    <*> ( Opt.option Opt.auto $
            Opt.long "num"
              <> Opt.short 'n'
              <> Opt.help "The number of fuzz runs that should be performed."
              <> Opt.showDefault
              <> Opt.value 1
              <> Opt.metavar "INT"
        )
    <*> ( Opt.switch $
            Opt.long "no-sim"
              <> Opt.help
                "Do not run simulation on the output netlist."
        )
    <*> ( Opt.switch $
            Opt.long "no-equiv"
              <> Opt.help
                "Do not run an equivalence check on the output netlist."
        )
    <*> ( Opt.switch $
            Opt.long "no-reduction"
              <> Opt.help
                "Do not run reduction on a failed testcase."
        )
    <*> textOption
      ( Opt.long "top"
          <> Opt.short 't'
          <> Opt.metavar "MODULE"
          <> Opt.help "Top module for the Verilog module."
          <> Opt.showDefault
          <> Opt.value "top"
      )
    <*> Opt.strArgument (Opt.metavar "FILE" <> Opt.help "Verilog input file to pass to EMI.")

genOpts :: Parser Opts
genOpts =
  Generate
    <$> ( Opt.optional
            . Opt.strOption
            $ Opt.long "output"
              <> Opt.short 'o'
              <> Opt.metavar "FILE"
              <> Opt.help "Output to a verilog file instead."
        )
    <*> ( Opt.optional
            . Opt.strOption
            $ Opt.long "config"
              <> Opt.short 'c'
              <> Opt.metavar "FILE"
              <> Opt.help "Config file for the generation run."
        )
    <*> ( Opt.switch $
            Opt.long "invalid"
              <> Opt.help
                "Generate invalid Verilog that is only syntactically allowed."
        )

parseOpts :: Parser Opts
parseOpts =
  Parse
    <$> ( fromText . T.pack
            <$> Opt.strArgument
              (Opt.metavar "FILE" <> Opt.help "Verilog input file.")
        )
    <*> ( Opt.optional
            . Opt.strOption
            $ Opt.long "output"
              <> Opt.short 'o'
              <> Opt.metavar "FILE"
              <> Opt.help "Output file to write the parsed file to."
        )
    <*> ( Opt.switch $
            Opt.long "strict"
              <> Opt.help
                "Makes the parser comply strictly to the Verilog 2005 standard."
        )

shuffleOpts :: Parser Opts
shuffleOpts =
  ShuffleOpt
    <$> ( fromText . T.pack
            <$> Opt.strArgument
              (Opt.metavar "FILE" <> Opt.help "Verilog input file.")
        )
    <*> textOption
      ( Opt.short 't'
          <> Opt.long "top"
          <> Opt.metavar "TOP"
          <> Opt.help "Name of top level module."
          <> Opt.showDefault
          <> Opt.value "top"
      )
    <*> ( Opt.optional
            . Opt.strOption
            $ Opt.long "output"
              <> Opt.short 'o'
              <> Opt.metavar "FILE"
              <> Opt.help "Output file to write the parsed file to."
        )
    <*> ( Opt.switch $
            Opt.long "no-shuffle-lines"
              <> Opt.help
                "Shuffle the lines in a Verilog file."
        )
    <*> ( Opt.switch $
            Opt.long "no-rename-vars"
              <> Opt.help
                "Rename the variables in a Verilog file."
        )
    <*> ( Opt.switch $
            Opt.long "noequiv"
              <> Opt.help
                "Do not check equivalence between input and output (currently only verismith generated Verilog is likely to pass this equivalence check)."
        )
    <*> ( Opt.strOption $
            Opt.long "equiv-output"
              <> Opt.short 'e'
              <> Opt.metavar "FOLDER"
              <> Opt.help "Output folder to write the equivalence checking files in."
              <> Opt.showDefault
              <> Opt.value "equiv"
        )
    <*> ( Opt.optional . textOption $
            Opt.long "checker"
              <> Opt.metavar "CHECKER"
              <> Opt.help "Define the checker to use."
        )

reduceOpts :: Parser Opts
reduceOpts =
  Reduce
    . fromText
    . T.pack
    <$> Opt.strArgument (Opt.metavar "FILE" <> Opt.help "Verilog input file.")
    <*> textOption
      ( Opt.short 't'
          <> Opt.long "top"
          <> Opt.metavar "TOP"
          <> Opt.help "Name of top level module."
          <> Opt.showDefault
          <> Opt.value "top"
      )
    <*> ( Opt.optional
            . Opt.strOption
            $ Opt.long "script"
              <> Opt.metavar "SCRIPT"
              <> Opt.help
                "Script that determines if the current file is interesting, which is determined by the script returning 0."
        )
    <*> ( Opt.many
            . Opt.option (optReader parseSynthDesc)
            $ Opt.short 's'
              <> Opt.long "synth"
              <> Opt.metavar "SYNTH"
              <> Opt.help "Specify synthesiser to use."
        )
    <*> ( Opt.switch $
            Opt.short 'r'
              <> Opt.long "rerun"
              <> Opt.help
                "Only rerun the current synthesis file with all the synthesisers."
        )

configOpts :: Parser Opts
configOpts =
  ConfigOpt
    <$> ( Opt.optional
            . Opt.strOption
            $ Opt.long "output"
              <> Opt.short 'o'
              <> Opt.metavar "FILE"
              <> Opt.help "Output to a TOML Config file."
        )
    <*> ( Opt.optional
            . Opt.strOption
            $ Opt.long "config"
              <> Opt.short 'c'
              <> Opt.metavar "FILE"
              <> Opt.help "Config file for the current fuzz run."
        )
    <*> ( Opt.switch $
            Opt.long "randomise"
              <> Opt.short 'r'
              <> Opt.help
                "Randomise the given default config, or the default config by randomly switchin on and off options."
        )

distanceOpts :: Parser Opts
distanceOpts =
  DistanceOpt
    <$> ( fromText . T.pack
            <$> Opt.strArgument
              (Opt.metavar "FILE" <> Opt.help "First verilog file.")
        )
    <*> ( fromText . T.pack
            <$> Opt.strArgument
              (Opt.metavar "FILE" <> Opt.help "Second verilog file.")
        )

equivOpts :: Parser Opts
equivOpts =
  Equiv
    <$> Opt.strOption
      ( Opt.long "output"
          <> Opt.short 'o'
          <> Opt.metavar "DIR"
          <> Opt.help "Output directory that the equivalence run takes place in."
          <> Opt.showDefault
          <> Opt.value "output"
      )
    <*> ( fromText . T.pack
            <$> Opt.strArgument
              (Opt.metavar "FILEA" <> Opt.help "First verilog file.")
        )
    <*> ( fromText . T.pack
            <$> Opt.strArgument
              (Opt.metavar "FILEB" <> Opt.help "Second verilog file.")
        )
    <*> textOption
      ( Opt.long "source-top"
          <> Opt.short 't'
          <> Opt.metavar "TOP"
          <> Opt.help "Define the top module to compare between the source files."
          <> Opt.showDefault
          <> Opt.value "top"
      )
    <*> ( Opt.optional . textOption $
            Opt.long "checker"
              <> Opt.metavar "CHECKER"
              <> Opt.help "Define the checker to use."
        )

argparse :: Parser Opts
argparse =
  Opt.hsubparser
    ( Opt.command
        "fuzz"
        ( Opt.info
            fuzzOpts
            ( Opt.progDesc
                "Run fuzzing on the specified simulators and synthesisers."
            )
        )
        <> Opt.metavar "fuzz"
    )
    <|> Opt.hsubparser
      ( Opt.command
          "emi"
          ( Opt.info
              emiOpts
              ( Opt.progDesc
                  "EMI testing using generated inputs, or existing Verilog designs."
              )
          )
          <> Opt.metavar "emi"
      )
    <|> Opt.hsubparser
      ( Opt.command
          "generate"
          ( Opt.info
              genOpts
              (Opt.progDesc "Generate a random Verilog program.")
          )
          <> Opt.metavar "generate"
      )
    <|> Opt.hsubparser
      ( Opt.command
          "parse"
          ( Opt.info
              parseOpts
              ( Opt.progDesc
                  "Parse a verilog file and output a pretty printed version."
              )
          )
          <> Opt.metavar "parse"
      )
    <|> Opt.hsubparser
      ( Opt.command
          "reduce"
          ( Opt.info
              reduceOpts
              ( Opt.progDesc
                  "Reduce a Verilog file by rerunning the fuzzer on the file."
              )
          )
          <> Opt.metavar "reduce"
      )
    <|> Opt.hsubparser
      ( Opt.command
          "shuffle"
          ( Opt.info
              shuffleOpts
              ( Opt.progDesc
                  "Shuffle a Verilog file."
              )
          )
          <> Opt.metavar "shuffle"
      )
    <|> Opt.hsubparser
      ( Opt.command
          "config"
          ( Opt.info
              configOpts
              ( Opt.progDesc
                  "Print the current configuration of the fuzzer."
              )
          )
          <> Opt.metavar "config"
      )
    <|> Opt.hsubparser
      ( Opt.command
          "distance"
          ( Opt.info
              distanceOpts
              ( Opt.progDesc
                  "Calculate the distance between two different pieces of Verilog."
              )
          )
          <> Opt.metavar "distance"
      )
    <|> Opt.hsubparser
      ( Opt.command
          "equiv"
          ( Opt.info
              equivOpts
              ( Opt.progDesc
                  "Check two different pieces of Verilog are equivalent."
              )
          )
          <> Opt.metavar "equiv"
      )

version :: Parser (a -> a)
version =
  Opt.infoOption versionInfo $
    mconcat
      [Opt.long "version", Opt.short 'v', Opt.help "Show version information.", Opt.hidden]

opts :: ParserInfo Opts
opts =
  Opt.info
    (argparse <**> Opt.helper <**> version)
    ( Opt.fullDesc
        <> Opt.progDesc "Fuzz different simulators and synthesisers."
        <> Opt.header
          "Verismith - A hardware simulator and synthesiser Verilog fuzzer."
    )
