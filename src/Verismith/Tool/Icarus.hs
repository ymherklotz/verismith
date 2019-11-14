{-|
Module      : Verismith.Tool.Icarus
Description : Icarus verilog module.
Copyright   : (c) 2018-2019, Yann Herklotz
License     : BSD-3
Maintainer  : yann [at] yannherklotz [dot] com
Stability   : experimental
Portability : POSIX

Icarus verilog module.
-}

module Verismith.Tool.Icarus
    ( Icarus(..)
    , defaultIcarus
    , runSimIc
    , runSimIcEC
    )
where

import           Control.DeepSeq            (NFData, rnf, rwhnf)
import           Control.Lens
import           Control.Monad              (void)
import           Crypto.Hash                (Digest, hash)
import           Crypto.Hash.Algorithms     (SHA256)
import           Data.Binary                (encode)
import           Data.Bits
import qualified Data.ByteArray             as BA (convert)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import           Data.ByteString.Lazy       (toStrict)
import qualified Data.ByteString.Lazy       as L (ByteString)
import           Data.Char                  (digitToInt)
import           Data.Foldable              (fold)
import           Data.List                  (transpose)
import           Data.Maybe                 (listToMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Numeric                    (readInt)
import           Prelude                    hiding (FilePath)
import           Shelly
import           Shelly.Lifted              (liftSh)
import           Verismith.CounterEg        (CounterEg (..))
import           Verismith.Result
import           Verismith.Tool.Internal
import           Verismith.Tool.Template
import           Verismith.Verilog.AST
import           Verismith.Verilog.BitVec
import           Verismith.Verilog.CodeGen
import           Verismith.Verilog.Internal
import           Verismith.Verilog.Mutate

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
    annotate (SimFail mempty) $ runSimWithFile sim "main.v" bss
    where m = rinfo ^. mainModule

runSimIcarusWithFile
    :: Icarus -> FilePath -> [ByteString] -> ResultSh ByteString
runSimIcarusWithFile sim f _ = annotate (SimFail mempty) . liftSh $ do
    dir <- pwd
    logCommand_ dir "icarus"
        $ run (icarusPath sim) ["-o", "main", toTextIgnore f]
    B.take 8 . BA.convert . (hash :: ByteString -> Digest SHA256) <$> logCommand
        dir
        "vvp"
        (runFoldLines (mempty :: ByteString) callback (vvpPath sim) ["main"])

fromBytes :: ByteString -> Integer
fromBytes = B.foldl' f 0 where f a b = a `shiftL` 8 .|. fromIntegral b

tbModule :: [ByteString] -> ModDecl -> Verilog
tbModule bss top =
    Verilog [ instantiateMod top $ ModDecl "testbench" [] []
              [ Initial
                $  fold [ BlockAssign (Assign "clk" Nothing 0)
                        , BlockAssign (Assign inConcat Nothing 0)
                        ]
                <> fold ((\r -> TimeCtrl 10
                             (Just $ BlockAssign (Assign inConcat Nothing r)))
                         . fromInteger . fromBytes <$> bss)
                <> (TimeCtrl 10 . Just . SysTaskEnable $ Task "finish" [])
              , Always . TimeCtrl 5 . Just $ BlockAssign
                (Assign "clk" Nothing (UnOp UnNot (Id "clk")))
              , Always . EventCtrl (EPosEdge "clk") . Just . SysTaskEnable
                $ Task "strobe" ["%b", Id "y"]
              ] []
            ]
  where
    inConcat = (RegConcat . filter (/= (Id "clk")) $ (Id . fromPort <$> (top ^. modInPorts)))

counterTestBench :: CounterEg -> ModDecl -> Verilog
counterTestBench (CounterEg _ states) m = tbModule filtered m
  where
    filtered = convert . fold . fmap snd . filter ((/= "clk") . fst) <$> states

runSimIc' :: (Synthesiser b) => ([ByteString] -> ModDecl -> Verilog)
          -> FilePath
          -> Icarus
          -> b
          -> SourceInfo
          -> [ByteString]
          -> Maybe ByteString
          -> ResultSh ByteString
runSimIc' fun datadir sim1 synth1 srcInfo bss bs = do
    dir <- liftSh pwd
    let top      = srcInfo ^. mainModule
    let tb = fun bss top
    liftSh . writefile tbname $ icarusTestbench datadir tb synth1
    liftSh $ exe dir "icarus" "iverilog" ["-o", exename, toTextIgnore tbname]
    s <- liftSh
        $   B.take 8
        .   BA.convert
        .   (hash :: ByteString -> Digest SHA256)
        <$> logCommand
                dir
                "vvp"
                (runFoldLines (mempty :: ByteString)
                              callback
                              (vvpPath sim1)
                              [exename])
    case (bs, s) of
        (Nothing, s') -> ResultT . return $ Pass s'
        (Just bs', s') -> if bs' == s'
                    then ResultT . return $ Pass s'
                    else ResultT . return $ Fail (SimFail s')
  where
    exe dir name e = void . errExit False . logCommand dir name . timeout e
    tbname = fromText $ toText synth1 <> "_testbench.v"
    exename = toText synth1 <> "_main"

runSimIc :: (Synthesiser b)
         => FilePath         -- ^ Data directory.
         -> Icarus           -- ^ Icarus simulator.
         -> b                -- ^ Synthesis tool to be tested.
         -> SourceInfo       -- ^ Original generated program to test.
         -> [ByteString]     -- ^ Test vectors to be passed as inputs to the generated Verilog.
         -> Maybe ByteString -- ^ What the correct output should be. If
                             -- 'Nothing' is passed, then just return 'Pass
                             -- ByteString' with the answer.
         -> ResultSh ByteString
runSimIc = runSimIc' tbModule

runSimIcEC :: (Synthesiser b) => FilePath -> Icarus -> b
           -> SourceInfo -> CounterEg -> Maybe ByteString -> ResultSh ByteString
runSimIcEC a b c d e = runSimIc' (const $ counterTestBench e) a b c d []
