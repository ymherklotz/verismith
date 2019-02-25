{-|
Module      : VeriFuzz.Reduce
Description : Test case reducer implementation.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Test case reducer implementation.
-}

module VeriFuzz.Reduce
    ( halveAssigns
    , reduce
    )
where

import           Control.Lens
import           VeriFuzz.AST

-- | Split a list in two halves.
halve :: [a] -> ([a], [a])
halve l = splitAt (length l `div` 2) l

-- | Split a module declaration in half by trying to remove assign statements.
halveAssigns :: VerilogSrc -> (VerilogSrc, VerilogSrc)
halveAssigns vsrc = (vsrc & vmod %~ fst . halve, vsrc & vmod %~ snd . halve)
    where
        vmod = getVerilogSrc . traverse . getDescription . modItems

-- | Reduce an input to a minimal representation.
reduce :: (VerilogSrc -> IO Bool) -- ^ Failed or not.
       -> VerilogSrc              -- ^ Input verilog source to be reduced.
       -> IO VerilogSrc           -- ^ Reduced output.
reduce eval src = do
    lresult <- eval l
    rresult <- eval r
    case (lresult, rresult) of
        (True, False) ->
            reduce eval l
        (False, True) ->
            reduce eval r
        (True, True) ->
            lreduced <- reduce eval l
            rreduced <- reduce eval r
            return lreduced
        _ ->
            return src
    where
        (l, r) = halveAssigns src
