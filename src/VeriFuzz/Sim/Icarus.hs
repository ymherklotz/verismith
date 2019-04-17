{-|
Module      : VeriFuzz.Sim.Icarus
Description : Icarus verilog module.
Copyright   : (c) 2018-2019, Yann Herklotz
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Icarus verilog module.
-}

module VeriFuzz.Sim.Icarus
    ( Icarus(..)
    , defaultIcarus
    )
where

import           Control.Lens
import           Crypto.Hash               (Digest, hash)
import           Crypto.Hash.Algorithms    (SHA256)
import           Data.Binary               (encode)
import qualified Data.ByteArray            as BA (convert)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B
import           Data.ByteString.Lazy      (toStrict)
import qualified Data.ByteString.Lazy      as L (ByteString)
import           Data.Char                 (digitToInt)
import           Data.Foldable             (fold)
import           Data.List                 (transpose)
import           Data.Maybe                (listToMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Numeric                   (readInt)
import           Prelude                   hiding (FilePath)
import           Shelly
import           Shelly.Lifted             (liftSh)
import           VeriFuzz.Sim.Internal
import           VeriFuzz.Verilog.AST
import           VeriFuzz.Verilog.BitVec
import           VeriFuzz.Verilog.CodeGen
import           VeriFuzz.Verilog.Internal
import           VeriFuzz.Verilog.Mutate

data Icarus = Icarus { icarusPath :: FilePath
                     , vvpPath    :: FilePath
                     }
              deriving (Eq, Show)

instance Tool Icarus where
  toText _ = "iverilog"

instance Simulator Icarus where
  runSim = runSimIcarus
  runSimWithFile = runSimIcarusWithFile

defaultIcarus :: Icarus
defaultIcarus = Icarus "iverilog" "vvp"

addDisplay :: [Statement] -> [Statement]
addDisplay s = concat $ transpose
    [ s
    , replicate l $ TimeCtrl 1 Nothing
    , replicate l . SysTaskEnable $ Task "display" ["%b", Id "y"]
    ]
    where l = length s

assignFunc :: [Port] -> ByteString -> Statement
assignFunc inp bs =
    NonBlockAssign
        . Assign conc Nothing
        . Number
        . BitVec (B.length bs * 8)
        $ bsToI bs
    where conc = RegConcat (portToExpr <$> inp)

convert :: Text -> ByteString
convert =
    toStrict
        . (encode :: Integer -> L.ByteString)
        . maybe 0 fst
        . listToMaybe
        . readInt 2 (`elem` ("01" :: String)) digitToInt
        . T.unpack

mask :: Text -> Text
mask = T.replace "x" "0"

callback :: ByteString -> Text -> ByteString
callback b t = b <> convert (mask t)

runSimIcarus :: Icarus -> SourceInfo -> [ByteString] -> ResultSh ByteString
runSimIcarus sim rinfo bss = do
    let tb = ModDecl
            "main"
            []
            []
            [ Initial
              $  fold (addDisplay $ assignFunc (_modInPorts m) <$> bss)
              <> (SysTaskEnable $ Task "finish" [])
            ]
            []
    let newtb     = instantiateMod m tb
    let modWithTb = Verilog [newtb, m]
    liftSh . writefile "main.v" $ genSource modWithTb
    annotate SimFail $ runSimWithFile sim "main.v" bss
    where m = rinfo ^. mainModule

runSimIcarusWithFile :: Icarus -> FilePath -> [ByteString] -> ResultSh ByteString
runSimIcarusWithFile sim f _ = annotate SimFail . liftSh $ do
    dir <- pwd
    echoP "Icarus: Compile"
    logger_ dir "icarus" $ run (icarusPath sim) ["-o", "main", toTextIgnore f]
    echoP "Icarus: Run"
    B.take 8 . BA.convert . (hash :: ByteString -> Digest SHA256) <$> logger
        dir
        "vvp"
        (runFoldLines (mempty :: ByteString) callback (vvpPath sim) ["main"])
