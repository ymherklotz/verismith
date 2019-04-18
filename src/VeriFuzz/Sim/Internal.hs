{-|
Module      : VeriFuzz.Sim.Internal
Description : Class of the simulator.
Copyright   : (c) 2018-2019, Yann Herklotz
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Class of the simulator and the synthesize tool.
-}

{-# LANGUAGE DeriveFunctor #-}

module VeriFuzz.Sim.Internal
    ( ResultSh
    , Tool(..)
    , Simulator(..)
    , Synthesiser(..)
    , Failed(..)
    , rootPath
    , timeout
    , timeout_
    , bsToI
    , noPrint
    , echoP
    , logger
    , logger_
    , execute
    , execute_
    , (<?>)
    , annotate
    )
where

import           Control.Monad         (void)
import           Data.Bits             (shiftL)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Time.LocalTime   (getZonedTime)
import           Prelude               hiding (FilePath)
import           Shelly
import           Shelly.Lifted         (MonadSh, liftSh)
import           System.FilePath.Posix (takeBaseName)
import           VeriFuzz.Internal
import           VeriFuzz.Result
import           VeriFuzz.Verilog.AST

-- | Tool class.
class Tool a where
  toText :: a -> Text

-- | Simulation type class.
class Tool a => Simulator a where
  runSim :: a             -- ^ Simulator instance
         -> SourceInfo       -- ^ Run information
         -> [ByteString]  -- ^ Inputs to simulate
         -> ResultSh ByteString -- ^ Returns the value of the hash at the output of the testbench.
  runSimWithFile :: a
                 -> FilePath
                 -> [ByteString]
                 -> ResultSh ByteString

data Failed = EmptyFail
            | EquivFail
            | SimFail
            | SynthFail
            deriving (Eq, Show)

instance Semigroup Failed where
    EmptyFail <> a = a
    b <> _ = b

instance Monoid Failed where
    mempty = EmptyFail

-- | Synthesiser type class.
class Tool a => Synthesiser a where
    runSynth :: a        -- ^ Synthesiser tool instance
             -> SourceInfo  -- ^ Run information
             -> FilePath -- ^ Output verilog file for the module
             -> ResultSh ()    -- ^ does not return any values
    synthOutput :: a -> FilePath
    setSynthOutput :: a -> FilePath -> a

-- | Type synonym for a 'ResultT' that will be used throughout 'VeriFuzz'. This
-- has instances for 'MonadSh' and 'MonadIO' if the 'Monad' it is parametrised
-- with also has those instances.
type ResultSh = ResultT Failed Sh

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
noPrint = liftSh . print_stdout False . print_stderr False
{-# INLINE noPrint #-}

echoP :: Text -> Sh ()
echoP t = do
    fn          <- pwd
    currentTime <- liftIO getZonedTime
    echo $ bname fn <> " [" <> showT currentTime <> "] - " <> t
    where bname = T.pack . takeBaseName . T.unpack . toTextIgnore

logger :: FilePath -> Text -> Sh a -> Sh a
logger fp name = log_stderr_with (l "_stderr.log") . log_stdout_with (l ".log")
  where
    l s t = appendFile (file s) (T.unpack t) >> appendFile (file s) "\n"
    file s = T.unpack (toTextIgnore $ fp </> fromText name) <> s

logger_ :: FilePath -> Text -> Sh a -> Sh ()
logger_ fp name = void . logger fp name

execute
    :: (MonadSh m, Monad m, Monoid a)
    => a
    -> FilePath
    -> Text
    -> FilePath
    -> [Text]
    -> ResultT a m Text
execute f dir name e = annotate f . liftSh . logger dir name . timeout e

execute_
    :: (MonadSh m, Monad m, Monoid a)
    => a
    -> FilePath
    -> Text
    -> FilePath
    -> [Text]
    -> ResultT a m ()
execute_ a b c d = void . execute a b c d
