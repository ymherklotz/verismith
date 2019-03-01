{-|
Module      : VeriFuzz.Internal.Simulator
Description : Class of the simulator.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Class of the simulator and the synthesize tool.
-}

module VeriFuzz.Internal.Simulator where

import           Control.Lens          ((^.), (^..))
import           Data.Bits             (shiftL)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Prelude               hiding (FilePath)
import           Shelly
import           System.FilePath.Posix (takeBaseName)
import           VeriFuzz.AST

-- | Tool class.
class Tool a where
  toText :: a -> Text

-- | Simulation type class.
class (Tool a) => Simulator a where
  runSim :: a             -- ^ Simulator instance
         -> SourceInfo       -- ^ Run information
         -> [ByteString]  -- ^ Inputs to simulate
         -> Sh ByteString -- ^ Returns the value of the hash at the output of the testbench.
  runSimWithFile :: a
                 -> FilePath
                 -> [ByteString]
                 -> Sh ByteString

-- | Synthesisor type class.
class (Tool a) => Synthesisor a where
  runSynth :: a        -- ^ Synthesisor tool instance
           -> SourceInfo  -- ^ Run information
           -> FilePath -- ^ Output verilog file for the module
           -> Sh ()    -- ^ does not return any values

data SourceInfo = SourceInfo { runMainModule :: {-# UNPACK #-} !Text
                             , runSource     :: !VerilogSrc
                             }

mainModule :: SourceInfo -> ModDecl
mainModule (SourceInfo main src) = head . filter ismain $ src ^.. getModule
    where ismain v = v ^. modId . getIdentifier == main

rootPath :: Sh FilePath
rootPath = do
    current <- pwd
    maybe current fromText <$> get_env "VERIFUZZ_ROOT"

timeout :: FilePath -> [Text] -> Sh Text
timeout = command1 "timeout" ["300"] . toTextIgnore
{-# INLINE timeout #-}

timeout_ :: FilePath -> [Text] -> Sh ()
timeout_ = command1_ "timeout" ["300"] . toTextIgnore
{-# INLINE timeout_ #-}

-- | Helper function to convert bytestrings to integers
bsToI :: ByteString -> Integer
bsToI = B.foldl' (\i b -> (i `shiftL` 8) + fromIntegral b) 0
{-# INLINE bsToI #-}

noPrint :: Sh a -> Sh a
noPrint = print_stdout False . print_stderr False
{-# INLINE noPrint #-}

echoP :: Text -> Sh ()
echoP t = do
    fn <- pwd
    echo $ bname fn <> " - " <> t
    where bname = T.pack . takeBaseName . T.unpack . toTextIgnore

logger :: FilePath -> Text -> Sh a -> Sh a
logger fp name = log_stderr_with (l "_log.stderr.txt")
    . log_stdout_with (l "_log.txt")
  where
    l s t = appendFile (file s) (T.unpack t) >> appendFile (file s) "\n"
    file s = T.unpack (toTextIgnore $ fp </> fromText name) <> s
