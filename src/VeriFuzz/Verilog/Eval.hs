{-|
Module      : VeriFuzz.Verilog.Eval
Description : Evaluation of Verilog expressions and statements.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Evaluation of Verilog expressions and statements.
-}

module VeriFuzz.Verilog.Eval
    ( evaluateConst
    )
where

import           Data.Bits
import           Data.Foldable           (fold)
import           Data.Functor.Foldable   (cata)
import           Data.Maybe              (fromMaybe, listToMaybe)
import           VeriFuzz.Verilog.AST
import           VeriFuzz.Verilog.BitVec

type Bindings = [Parameter]

paramIdent_ :: Parameter -> Identifier
paramIdent_ (Parameter i _) = i

paramValue_ :: Parameter -> ConstExpr
paramValue_ (Parameter _ v) = v

applyUnary :: (Num a, FiniteBits a) => UnaryOperator -> a -> a
applyUnary UnPlus  a = a
applyUnary UnMinus a = negate a
applyUnary UnLNot a | a == 0    = 0
                    | otherwise = 1
applyUnary UnNot a = complement a
applyUnary UnAnd a | finiteBitSize a == popCount a = 1
                   | otherwise                     = 0
applyUnary UnNand a | finiteBitSize a == popCount a = 0
                    | otherwise                     = 1
applyUnary UnOr a | popCount a == 0 = 0
                  | otherwise       = 1
applyUnary UnNor a | popCount a == 0 = 1
                   | otherwise       = 0
applyUnary UnXor a | popCount a `mod` 2 == 0 = 0
                   | otherwise               = 1
applyUnary UnNxor a | popCount a `mod` 2 == 0 = 1
                    | otherwise               = 0
applyUnary UnNxorInv a | popCount a `mod` 2 == 0 = 1
                       | otherwise               = 0

compXor :: Bits c => c -> c -> c
compXor a = complement . xor a

toIntegral :: Num p => (t1 -> t2 -> Bool) -> t1 -> t2 -> p
toIntegral a b c = if a b c then 1 else 0

toInt :: (Integral a, Num t1) => (t2 -> t1 -> t3) -> t2 -> a -> t3
toInt a b c = a b $ fromIntegral c

applyBinary :: (Integral a, Bits a) => BinaryOperator -> a -> a -> a
applyBinary BinPlus    = (+)
applyBinary BinMinus   = (-)
applyBinary BinTimes   = (*)
applyBinary BinDiv     = quot
applyBinary BinMod     = rem
applyBinary BinEq      = toIntegral (==)
applyBinary BinNEq     = toIntegral (/=)
applyBinary BinCEq     = toIntegral (==)
applyBinary BinCNEq    = toIntegral (/=)
applyBinary BinLAnd    = undefined
applyBinary BinLOr     = undefined
applyBinary BinLT      = toIntegral (<)
applyBinary BinLEq     = toIntegral (<=)
applyBinary BinGT      = toIntegral (>)
applyBinary BinGEq     = toIntegral (>=)
applyBinary BinAnd     = (.&.)
applyBinary BinOr      = (.|.)
applyBinary BinXor     = xor
applyBinary BinXNor    = compXor
applyBinary BinXNorInv = compXor
applyBinary BinPower   = undefined
applyBinary BinLSL     = toInt shiftL
applyBinary BinLSR     = toInt shiftR
applyBinary BinASL     = toInt shiftL
applyBinary BinASR     = toInt shiftR

-- | Evaluates a 'ConstExpr' using a context of 'Bindings' as input.
evaluateConst :: Bindings -> ConstExprF BitVec -> BitVec
evaluateConst _ (ConstNumF b) = b
evaluateConst p (ParamIdF i) =
    cata (evaluateConst p)
        . fromMaybe 0
        . fmap paramValue_
        . listToMaybe
        $ filter ((== i) . paramIdent_) p
evaluateConst _ (ConstConcatF c       ) = fold c
evaluateConst _ (ConstUnOpF unop c    ) = applyUnary unop c
evaluateConst _ (ConstBinOpF a binop b) = applyBinary binop a b
evaluateConst _ (ConstCondF  a b     c) = if a > 0 then b else c
evaluateConst _ (ConstStrF _          ) = 0
