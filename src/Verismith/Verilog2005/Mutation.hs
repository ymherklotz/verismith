-- Module      : Verismith.Verilog2005.Mutation
-- Description : Mutating a Verilog AST.
-- Copyright   : (c) 2023-2024 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : experimental
-- Portability : POSIX

module Verismith.Verilog2005.Mutation
  ( renameTopItems,
  )
where

import Control.Lens
import Data.ByteString (ByteString)
import Data.Data.Lens
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty (..))
import Verismith.Verilog2005.AST
import Verismith.Verilog2005.Utils

-- | Rename all top-level scopes and everything depending on their name
renameTopItems :: (ByteString -> ByteString) -> Verilog2005 -> Verilog2005
renameTopItems fb (Verilog2005 m p c) =
  Verilog2005
    ( map
        ( (mbIdent %~ f)
            . ( transformOn biplate $ \mgi -> case mgi of
                  MGIUDPInst t s d l -> MGIUDPInst (nmap t) s d l
                  MGIModInst t p l -> MGIModInst (nmap t) p l
                  MGIUnknownInst t p l -> MGIUnknownInst (nmap t) p l
                  _ -> mgi :: ModGenBlockedItem
              )
            . (biplate . hiPath . _head . _1 %~ nmap)
        )
        m
    )
    (p & each . pbIdent %~ f)
    ( map
        ( (cbIdent %~ f)
            . (biplate %~ \(Dot1Ident l c) -> Dot1Ident l $ nmap c)
            . (cbBody . each . ciCell_inst . _CIInst %~ \(h :| t) -> nmap h :| t)
        )
        c
    )
  where
    f (Identifier b) = Identifier $ fb b
    nmap (Identifier x) = Identifier $ HashMap.findWithDefault x x mapmap
    mapmap =
      HashMap.fromList $
        map (\(Identifier x) -> (x, fb x)) $
          m ^.. each . mbIdent ++ p ^.. each . pbIdent ++ c ^.. each . cbIdent
