{-|
Module      : Verismith.Tool.Internal
Description : Class of the simulator.
Copyright   : (c) 2018-2019, Yann Herklotz
License     : GPLv3
Maintainer  : yann [at] yannherklotz [dot] com
Stability   : experimental
Portability : POSIX

Class of the simulator and the synthesize tool.
-}

{-# LANGUAGE DeriveFunctor #-}

module Verismith.Tool.Internal
    ( ResultSh
    , resultSh
    , Tool(..)
    , Simulator(..)
    , Synthesiser(..)
    , Failed(..)
    , renameSource
    , checkPresent
    , checkPresentModules
    , replace
    , replaceMods
    , rootPath
    , timeout
    , timeout_
    , bsToI
    , noPrint
    , logger
    , logCommand
    , logCommand_
    , execute
    , execute_
    , (<?>)
    , annotate
    )
where

import           Control.Lens
import           Control.Monad         (forM, void)
import           Control.Monad.Catch   (throwM)
import           Data.Bits             (shiftL)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import           Data.Maybe            (catMaybes)
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Time.Format      (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime   (getZonedTime)
import           Prelude               hiding (FilePath)
import           Shelly
import           Shelly.Lifted         (MonadSh, liftSh)
import           System.FilePath.Posix (takeBaseName)
import           Verismith.CounterEg   (CounterEg)
import           Verismith.Internal
import           Verismith.Result
import           Verismith.Verilog.AST

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
            | EquivFail (Maybe CounterEg)
            | EquivError
            | SimFail ByteString
            | SynthFail
            | TimeoutError
            deriving (Eq)

instance Show Failed where
    show EmptyFail     = "EmptyFail"
    show (EquivFail _) = "EquivFail"
    show EquivError    = "EquivError"
    show (SimFail bs)  = "SimFail " <> T.unpack (T.take 10 $ showBS bs)
    show SynthFail     = "SynthFail"
    show TimeoutError  = "TimeoutError"

instance Semigroup Failed where
    EmptyFail <> a = a
    b <> _ = b

instance Monoid Failed where
    mempty = EmptyFail

-- | Synthesiser type class.
class Tool a => Synthesiser a where
    runSynth :: a        -- ^ Synthesiser tool instance
             -> SourceInfo  -- ^ Run information
             -> ResultSh ()    -- ^ does not return any values
    synthOutput :: a -> FilePath
    setSynthOutput :: a -> FilePath -> a

renameSource :: (Synthesiser a) => a -> SourceInfo -> SourceInfo
renameSource a src =
    src & infoSrc . _Wrapped . traverse . modId . _Wrapped %~ (<> toText a)

-- | Type synonym for a 'ResultT' that will be used throughout 'Verismith'. This
-- has instances for 'MonadSh' and 'MonadIO' if the 'Monad' it is parametrised
-- with also has those instances.
type ResultSh = ResultT Failed Sh

resultSh :: ResultSh a -> Sh a
resultSh s = do
    result <- runResultT s
    case result of
        Fail e  -> throwM . RunFailed "" [] 1 $ showT e
        Pass s' -> return s'

checkPresent :: FilePath -> Text -> Sh (Maybe Text)
checkPresent fp t = do
    errExit False $ run_ "grep" [t, toTextIgnore fp]
    i <- lastExitCode
    if i == 0 then return $ Just t else return Nothing

-- | Checks what modules are present in the synthesised output, as some modules
-- may have been inlined. This could be improved if the parser worked properly.
checkPresentModules :: FilePath -> SourceInfo -> Sh [Text]
checkPresentModules fp (SourceInfo _ src) = do
    vals <- forM (src ^.. _Wrapped . traverse . modId . _Wrapped)
        $ checkPresent fp
    return $ catMaybes vals

-- | Uses sed to replace a string in a text file.
replace :: FilePath -> Text -> Text -> Sh ()
replace fp t1 t2 = do
    errExit False . noPrint $ run_
        "sed"
        ["-i", "s/" <> t1 <> "/" <> t2 <> "/g", toTextIgnore fp]

-- | This is used because rename only renames the definitions of modules of
-- course, so instead this just searches and replaces all the module names. This
-- should find all the instantiations and definitions. This could again be made
-- much simpler if the parser works.
replaceMods :: FilePath -> Text -> SourceInfo -> Sh ()
replaceMods fp t (SourceInfo _ src) =
    void
        . forM (src ^.. _Wrapped . traverse . modId . _Wrapped)
        $ (\a -> replace fp a (a <> t))

rootPath :: Sh FilePath
rootPath = do
    current <- pwd
    maybe current fromText <$> get_env "VERISMITH_ROOT"

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

logger :: Text -> Sh ()
logger t = do
    fn          <- pwd
    currentTime <- liftIO getZonedTime
    echo
        $  "Verismith "
        <> T.pack (formatTime defaultTimeLocale "%H:%M:%S " currentTime)
        <> bname fn
        <> " - "
        <> t
    where bname = T.pack . takeBaseName . T.unpack . toTextIgnore

logCommand :: FilePath -> Text -> Sh a -> Sh a
logCommand fp name = log_stderr_with (l "_stderr.log")
    . log_stdout_with (l ".log")
  where
    l s t = appendFile (file s) (T.unpack t) >> appendFile (file s) "\n"
    file s = T.unpack (toTextIgnore $ fp </> fromText name) <> s

logCommand_ :: FilePath -> Text -> Sh a -> Sh ()
logCommand_ fp name = void . logCommand fp name

execute
    :: (MonadSh m, Monad m)
    => Failed
    -> FilePath
    -> Text
    -> FilePath
    -> [Text]
    -> ResultT Failed m Text
execute f dir name e cs = do
    (res, exitCode) <- liftSh $ do
        res <- errExit False . logCommand dir name $ timeout e cs
        (,) res <$> lastExitCode
    case exitCode of
        0   -> ResultT . return $ Pass res
        124 -> ResultT . return $ Fail TimeoutError
        _   -> ResultT . return $ Fail f

execute_
    :: (MonadSh m, Monad m)
    => Failed
    -> FilePath
    -> Text
    -> FilePath
    -> [Text]
    -> ResultT Failed m ()
execute_ a b c d = void . execute a b c d
