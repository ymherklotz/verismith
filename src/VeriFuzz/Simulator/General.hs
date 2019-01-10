{-|
Module      : VeriFuzz.Simulator.General
Description : Class of the simulator.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Class of the simulator and the synthesize tool.
-}

module VeriFuzz.Simulator.General where

import           Data.Bits            (shiftL)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as B
import           Data.Text            (Text)
import           Prelude              hiding (FilePath)
import           Shelly
import           VeriFuzz.Verilog.AST

-- | Simulator class.
class Simulator a where
  toText :: a -> Text

-- | Simulation type class.
class (Simulator a) => Simulate a where
  runSim :: a            -- ^ Simulator instance
         -> ModDecl      -- ^ Module to simulate
         -> [ByteString] -- ^ Inputs to simulate
         -> Sh Int       -- ^ Returns the value of the hash at the output of the testbench.

-- | Synthesize type class.
class (Simulator a) => Synthesize a where
  runSynth :: a        -- ^ Synthesize tool instance
           -> ModDecl  -- ^ Module to synthesize
           -> FilePath -- ^ Output verilog file for the module
           -> Sh ()    -- ^ does not return any values

timeout :: FilePath -> [Text] -> Sh Text
timeout = command1 "timeout" ["180"] . toTextIgnore

timeout_ :: FilePath -> [Text] -> Sh ()
timeout_ = command1_ "timeout" ["180"] . toTextIgnore

-- | Helper function to convert bytestrings to integers
bsToI :: ByteString -> Integer
bsToI = B.foldl' (\i b -> (i `shiftL` 8) + fromIntegral b) 0
{-# INLINE bsToI #-}
