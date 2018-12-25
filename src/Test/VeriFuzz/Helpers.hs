{-|
Module      : Test.VeriFuzz.Default
Description : Defaults and common functions.
Copyright   : (c) Yann Herklotz Grave 2018
License     : GPL-3
Maintainer  : ymherklotz@gmail.com
Stability   : experimental
Portability : POSIX

Defaults and common functions.
-}

{-# LANGUAGE OverloadedStrings #-}

module Test.VeriFuzz.Helpers where

import           Control.Lens
import           Data.Text                (Text)
import qualified Data.Text
import           Test.VeriFuzz.VerilogAST

regDecl :: Text -> ModItem
regDecl = Decl . Port Nothing (Just $ Reg False) . Identifier

wireDecl :: Text -> ModItem
wireDecl = Decl . Port Nothing (Just $ PortNet Wire) . Identifier

modConn :: Text -> ModConn
modConn = ModConn . PrimExpr . PrimId . Identifier

-- | Create a number expression which will be stored in a primary expression.
numExpr :: Int -> Int -> Expression
numExpr = ((PrimExpr . PrimNum) .) . Number

-- | Create an empty module.
emptyMod :: ModDecl
emptyMod = ModDecl (Identifier "") [] []

-- | Set a module name for a module declaration.
setModName :: Text -> ModDecl -> ModDecl
setModName str = moduleId .~ Identifier str

-- | Add a port to the module declaration.
addModPort :: Port -> ModDecl -> ModDecl
addModPort port = modPorts %~ (:) port

addDescription :: Description -> SourceText -> SourceText
addDescription desc = getSourceText %~ (:) desc

testBench :: ModDecl
testBench =
  ModDecl "main" []
  [ regDecl "a"
  , regDecl "b"
  , wireDecl "c"
  , ModInst "and" "and_gate"
    [ modConn "c"
    , modConn "a"
    , modConn "b"
    ]
  , Initial $ SeqBlock
    [ BlockAssign . Assign (RegId "a") Nothing . PrimExpr . PrimNum $ Number 1 1
    , BlockAssign . Assign (RegId "b") Nothing . PrimExpr . PrimNum $ Number 1 1
    -- , TimeCtrl (Delay 1) . Just . SysTaskEnable $ Task "display"
    --   [ ExprStr "%d & %d = %d"
    --   , PrimExpr $ PrimId "a"
    --   , PrimExpr $ PrimId "b"
    --   , PrimExpr $ PrimId "c"
    --   ]
    -- , SysTaskEnable $ Task "finish" []
    ]
  ]

addTestBench :: SourceText -> SourceText
addTestBench = addDescription $ Description testBench
