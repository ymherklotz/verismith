{-|
Module      : VeriFuzz.Internal
Description : Shared high level code used in the other modules internally.
Copyright   : (c) 2018-2019, Yann Herklotz Grave
License     : BSD-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Shared high level code used in the other modules internally.
-}

module VeriFuzz.Internal
  ( -- * Useful functions
    safe
    -- * Circuit modules
  , module VeriFuzz.Internal.Circuit
    -- * Simulator Internals
  , module VeriFuzz.Internal.Simulator
  ) where

import           VeriFuzz.Internal.Circuit
import           VeriFuzz.Internal.Simulator

-- | Converts unsafe list functions in the Prelude to a safe version.
safe :: ([a] -> b) -> [a] -> Maybe b
safe _ [] = Nothing
safe f l  = Just $ f l
