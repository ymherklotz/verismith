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

import           Control.DeepSeq           (NFData, rnf, rwhnf)
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
              deriving (Eq)

instance Show Icarus where
    show _ = "iverilog"

instance Tool Icarus where
  toText _ = "iverilog"

instance Simulator Icarus where
  runSim = runSimIcarus
  runSimWithFile = runSimIcarusWithFile

instance NFData Icarus where
    rnf = rwhnf

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

runSimIcarusWithFile
    :: Icarus -> FilePath -> [ByteString] -> ResultSh ByteString
runSimIcarusWithFile sim f _ = annotate SimFail . liftSh $ do
    dir <- pwd
    logCommand_ dir "icarus"
        $ run (icarusPath sim) ["-o", "main", toTextIgnore f]
    B.take 8 . BA.convert . (hash :: ByteString -> Digest SHA256) <$> logCommand
        dir
        "vvp"
        (runFoldLines (mempty :: ByteString) callback (vvpPath sim) ["main"])

runSimIc
    :: (Synthesiser a) => a -> SourceInfo -> [ByteString] -> ResultSh ByteString
runSimIc sim1 srcInfo bss = do
    dir <- liftSh pwd
    let top = srcInfo ^. mainModule
    let inConcat = (RegConcat (Id . fromPort <$> (top ^. modInPorts)))
    let tb = ModDecl
             "testbench"
             []
             []
             [ Initial
               $ fold [ BlockAssign (Assign "clk" Nothing 0)
                      , BlockAssign (Assign  inConcat Nothing 0)
                      ]
               <> (SysTaskEnable $ Task "finish" [])
             , Always . TimeCtrl 5 $ BlockAssign (Assign "clk" Nothing (UnNot (Id "clk")))
             , Always . EventCtrl (EPosEdge "clk") $ SysTaskEnable $ Task "strobe" ["%b", Id "y"]
             ]
             []
    liftSh $ do
        writefile "top.v"
            .  genSource
            .  initMod
            .  makeTopAssert
            $  srcInfo
            ^. mainModule
        replaceMods (synthOutput sim1) "_1" srcInfo
        replaceMods (synthOutput sim2) "_2" srcInfo
        writefile "proof.sby" $ sbyConfig sim1 sim2 srcInfo
    e <- liftSh $ do
        exe dir "symbiyosys" "sby" ["-f", "proof.sby"]
        lastExitCode
    case e of
        0   -> ResultT . return $ Pass ()
        2   -> ResultT . return $ Fail EquivFail
        124 -> ResultT . return $ Fail TimeoutError
        _   -> ResultT . return $ Fail EquivError
  where
    exe dir name e = void . errExit False . logCommand dir name . timeout e
