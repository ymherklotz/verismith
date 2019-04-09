{-|
Module      : VeriFuzz.Verilog
Description : Verilog implementation with random generation and mutations.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Verilog implementation with random generation and mutations.
-}

module VeriFuzz.Verilog
    ( Verilog(..)
    , parseVerilog
    , procedural
    , randomMod
    , GenVerilog(..)
    , genSource
    , getVerilog
    -- * Primitives
    -- ** Identifier
    , Identifier(..)
    , getIdentifier
    -- ** Control
    , Delay(..)
    , getDelay
    , Event(..)
    -- ** Operators
    , BinaryOperator(..)
    , UnaryOperator(..)
    -- ** Task
    , Task(..)
    , taskName
    , taskExpr
    -- ** Left hand side value
    , LVal(..)
    , regId
    , regExprId
    , regExpr
    , regSizeId
    , regSizeMSB
    , regSizeLSB
    , regConc
    -- ** Ports
    , PortDir(..)
    , PortType(..)
    , Port(..)
    , portType
    , portSigned
    , portSize
    , portName
    -- * Expression
    , Expr(..)
    , exprSize
    , exprVal
    , exprId
    , exprConcat
    , exprUnOp
    , exprPrim
    , exprLhs
    , exprBinOp
    , exprRhs
    , exprCond
    , exprTrue
    , exprFalse
    , exprFunc
    , exprBody
    , exprStr
    , ConstExpr(..)
    , constNum
    , Function(..)
    -- * Assignment
    , Assign(..)
    , assignReg
    , assignDelay
    , assignExpr
    , ContAssign(..)
    , contAssignNetLVal
    , contAssignExpr
    -- * Statment
    , Statement(..)
    , statDelay
    , statDStat
    , statEvent
    , statEStat
    , statements
    , stmntBA
    , stmntNBA
    , stmntTask
    , stmntSysTask
    , stmntCondExpr
    , stmntCondTrue
    , stmntCondFalse
    -- * Module
    , ModDecl(..)
    , modId
    , modOutPorts
    , modInPorts
    , modItems
    , ModItem(..)
    , modContAssign
    , modInstId
    , modInstName
    , modInstConns
    , traverseModItem
    , declDir
    , declPort
    , ModConn(..)
    , modConn
    , modConnName
    , modExpr
    -- * Useful Lenses and Traversals
    , getModule
    , getSourceId
    -- * Arbitrary
    , Arb
    , arb
    , genPositive
    , exprWithContext
    )
where

import           VeriFuzz.Verilog.Arbitrary
import           VeriFuzz.Verilog.AST
import           VeriFuzz.Verilog.CodeGen
import           VeriFuzz.Verilog.Gen
import           VeriFuzz.Verilog.Parser
