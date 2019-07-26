module Main where

import           Criterion (benchmark, nfAppIO)
import           VeriFuzz

main :: IO ()
main = benchmark $ nfAppIO (proceduralIO "top") defaultConfig
