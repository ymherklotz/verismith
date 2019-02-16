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
  , showT
  , comma
    -- * Module Specific Internals
  , module VeriFuzz.Internal.Circuit
  , module VeriFuzz.Internal.Simulator
  , module VeriFuzz.Internal.AST
  ) where

import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           VeriFuzz.Internal.AST
import           VeriFuzz.Internal.Circuit
import           VeriFuzz.Internal.Simulator

-- | Converts unsafe list functions in the Prelude to a safe version.
safe :: ([a] -> b) -> [a] -> Maybe b
safe _ [] = Nothing
safe f l  = Just $ f l

-- | Show function for 'Text'
showT :: (Show a) => a -> Text
showT = T.pack . show

-- | Inserts commas between '[Text]' and except the last one.
comma :: [Text] -> Text
comma = T.intercalate ", "
