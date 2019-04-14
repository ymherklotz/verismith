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
    , proceduralIO
    , randomMod
    , GenVerilog(..)
    , genSource
    -- * Primitives
    -- ** Identifier
    , Identifier(..)
    -- ** Control
    , Delay(..)
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
    , regSizeRange
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
    , ConstExpr(..)
    , constNum
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
    )
where

import           VeriFuzz.Verilog.AST
import           VeriFuzz.Verilog.CodeGen
import           VeriFuzz.Verilog.Gen
import           VeriFuzz.Verilog.Parser
