{-|
Module      : Main
Description : Main Yosys runner
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Main Yosys runner
-}

module Main where

import           Prelude hiding (FilePath)
import           Shelly

main :: IO ()
main = putStrLn "Hello"
