module Main where

import           Control.Concurrent
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
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

data Opts = Fuzz { fuzzOutput :: Text
                 }
          | Rerun { tool :: Tool
                  }
          | Generate { fileName :: S.FilePath
                     }

myForkIO :: IO () -> IO (MVar ())
myForkIO io = do
  mvar <- newEmptyMVar
  _ <- forkFinally io (\_ -> putMVar mvar ())
  return mvar

textOption :: Mod OptionFields String -> Parser Text
textOption = fmap T.pack . strOption

optReader :: (String -> Maybe a) -> ReadM a
optReader f = eitherReader $ \arg ->
  case f arg of
    Just a  -> Right a
    Nothing -> Left $ "Cannot parse option: " <> arg

parseSynth :: String -> Maybe Tool
parseSynth val
  | val == "yosys" = Just Yosys
  | val == "xst"= Just XST
  | otherwise = Nothing

parseSim :: String -> Maybe Tool
parseSim val
  | val == "icarus" = Just Icarus
  | otherwise = Nothing

fuzzOpts :: Parser Opts
fuzzOpts = Fuzz
  <$> textOption
  ( long "output"
    <> short 'o'
    <> metavar "DIR"
    <> help "Output directory that the fuzz run takes place in."
    <> showDefault
    <> value "output"
  )

rerunOpts :: Parser Opts
rerunOpts = Rerun
  <$> ( option (optReader parseSynth)
        ( long "synth"
          <> metavar "SYNTH"
          <> help "Rerun using a synthesiser (yosys|xst)."
          <> showDefault
          <> value Yosys
        )
        <|> option (optReader parseSim)
        ( long "sim"
          <> metavar "SIM"
          <> help "Rerun using a simulator (icarus)."
          <> showDefault
          <> value Icarus
        )
      )

genOpts :: Parser Opts
genOpts = Generate . S.fromText <$> textOption
  ( long "output"
    <> short 'o'
    <> metavar "FILE"
    <> help "Verilog output file."
    <> showDefault
    <> value "main.v"
  )

argparse :: Parser Opts
argparse =
  hsubparser (command "fuzz"
              (info fuzzOpts
               (progDesc "Run fuzzing on the specified simulators and synthesisers."))
              <> metavar "fuzz")
  <|> hsubparser (command "rerun"
                  (info rerunOpts
                   (progDesc "Rerun a Verilog file with a simulator or a synthesiser."))
                  <> metavar "rerun")
  <|> hsubparser (command "generate"
                  (info genOpts
                   (progDesc "Generate a random Verilog program."))
                  <> metavar "generate")

opts :: ParserInfo Opts
opts = info (argparse <**> helper)
       ( fullDesc
         <> progDesc "Fuzz different simulators and synthesisers."
         <> header "VeriFuzz - A hardware simulator and synthesiser Verilog fuzzer." )

handleOpts :: Opts -> IO ()
handleOpts (Fuzz a) = do
  num <- getNumCapabilities
  vars <- sequence $ (\x -> myForkIO $
                       V.runEquivalence (V.randomMod 10 100)
                       ("test_" <> T.pack (show x)) 0) <$> [1..num]
  sequence_ $ takeMVar <$> vars
handleOpts (Generate f) = do
  g <- QC.generate $ V.randomMod 5 15
  S.shelly . S.writefile f $ V.genSource g
handleOpts (Rerun f) = undefined

main :: IO ()
 --main = sample (arbitrary :: Gen (Circuit Input))
main = do
  optsparsed <- execParser opts
  handleOpts optsparsed

