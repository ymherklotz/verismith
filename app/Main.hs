module Main where

import           Control.Concurrent
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Options.Applicative
import           Prelude             hiding (FilePath)
import           Simulation
import           VeriFuzz

data Opts = Opts
  { output :: Text }

myForkIO :: IO () -> IO (MVar ())
myForkIO io = do
  mvar <- newEmptyMVar
  _ <- forkFinally io (\_ -> putMVar mvar ())
  return mvar

textOption :: Mod OptionFields String -> Parser Text
textOption = fmap T.pack . strOption

argparse :: Parser Opts
argparse = Opts
  <$> textOption
  ( long "output"
    <> short 'o'
    <> metavar "DIR"
    <> help "Output directory that the fuzz run takes place in."
    <> showDefault
    <> value "output"
  )

opts :: ParserInfo Opts
opts = info (argparse <**> helper)
       ( fullDesc
         <> progDesc "Fuzz different simulators and synthesisers."
         <> header "VeriFuzz - A hardware simulator and synthesiser Verilog fuzzer." )

main :: IO ()
 --main = sample (arbitrary :: Gen (Circuit Input))
main = do
  optsparsed <- execParser opts
  num <- getNumCapabilities
  vars <- sequence $ (\x -> myForkIO $
                       runEquivalence (randomMod 5 15) ("test_" <> T.pack (show x)) 0) <$> [1..num]
  sequence_ $ takeMVar <$> vars
