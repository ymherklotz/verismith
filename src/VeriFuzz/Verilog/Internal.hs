{-|
Module      : VeriFuzz.Verilog.Internal
Description : Defaults and common functions.
Copyright   : (c) 2018-2019, Yann Herklotz
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Defaults and common functions.
-}

module VeriFuzz.Verilog.Internal
    ( regDecl
    , wireDecl
    , emptyMod
    , setModName
    , addModPort
    , addModDecl
    , testBench
    , addTestBench
    , defaultPort
    , portToExpr
    , modName
    , yPort
    , wire
    , reg
    )
where

import           Control.Lens
import           Data.Text            (Text)
import           VeriFuzz.Verilog.AST

regDecl :: Identifier -> ModItem
regDecl = Decl Nothing . Port Reg False 1

wireDecl :: Identifier -> ModItem
wireDecl = Decl Nothing . Port Wire False 1

-- | Create an empty module.
emptyMod :: ModDecl
emptyMod = ModDecl "" [] [] [] []

-- | Set a module name for a module declaration.
setModName :: Text -> ModDecl -> ModDecl
setModName str = modId .~ Identifier str

-- | Add a input port to the module declaration.
addModPort :: Port -> ModDecl -> ModDecl
addModPort port = modInPorts %~ (:) port

addModDecl :: ModDecl -> Verilog -> Verilog
addModDecl desc = getVerilog %~ (:) desc

testBench :: ModDecl
testBench = ModDecl
    "main"
    []
    []
    [ regDecl "a"
    , regDecl "b"
    , wireDecl "c"
    , ModInst "and"
              "and_gate"
              [ModConn $ Id "c", ModConn $ Id "a", ModConn $ Id "b"]
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
    ] []

addTestBench :: Verilog -> Verilog
addTestBench = addModDecl testBench

defaultPort :: Identifier -> Port
defaultPort = Port Wire False 1

portToExpr :: Port -> Expr
portToExpr (Port _ _ _ i) = Id i

modName :: ModDecl -> Text
modName = view $ modId . getIdentifier

yPort :: Identifier -> Port
yPort = Port Wire False 90

wire :: Int -> Identifier -> Port
wire = Port Wire False

reg :: Int -> Identifier -> Port
reg = Port Reg False
