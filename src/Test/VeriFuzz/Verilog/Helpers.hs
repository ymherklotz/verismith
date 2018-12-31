{-|
Module      : Test.VeriFuzz.Verilog.Helpers
Description : Defaults and common functions.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Defaults and common functions.
-}

module Test.VeriFuzz.Verilog.Helpers where

import           Control.Lens
import           Data.Text                 (Text)
import qualified Data.Text
import           Test.VeriFuzz.Verilog.AST

regDecl :: Identifier -> ModItem
regDecl = Decl Nothing . Port (Reg False) 1

wireDecl :: Identifier -> ModItem
wireDecl = Decl Nothing . Port Wire 1

modConn :: Identifier -> ModConn
modConn = ModConn . Id

-- | Create an empty module.
emptyMod :: ModDecl
emptyMod = ModDecl "" [] [] []

-- | Set a module name for a module declaration.
setModName :: Text -> ModDecl -> ModDecl
setModName str = moduleId .~ Identifier str

-- | Add a input port to the module declaration.
addModPort :: Port -> ModDecl -> ModDecl
addModPort port = modInPorts %~ (:) port

addDescription :: Description -> VerilogSrc -> VerilogSrc
addDescription desc = getVerilogSrc %~ (:) desc

testBench :: ModDecl
testBench =
  ModDecl "main" [] []
  [ regDecl "a"
  , regDecl "b"
  , wireDecl "c"
  , ModInst "and" "and_gate"
    [ modConn "c"
    , modConn "a"
    , modConn "b"
    ]
  , Initial $ SeqBlock
    [ BlockAssign . Assign (RegId "a") Nothing $ Number 1 1
    , BlockAssign . Assign (RegId "b") Nothing $ Number 1 1
    -- , TimeCtrl (Delay 1) . Just . SysTaskEnable $ Task "display"
    --   [ Str "%d & %d = %d"
    --   , PrimExpr $ PrimId "a"
    --   , PrimExpr $ PrimId "b"
    --   , PrimExpr $ PrimId "c"
    --   ]
    -- , SysTaskEnable $ Task "finish" []
    ]
  ]

addTestBench :: VerilogSrc -> VerilogSrc
addTestBench = addDescription $ Description testBench

defaultPort :: Identifier -> Port
defaultPort = Port Wire 1
