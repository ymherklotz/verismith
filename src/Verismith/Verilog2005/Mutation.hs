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
renameTopItems f (Verilog2005 m po p c) =
  Verilog2005
    ( map
        ( (mbIdent %~ f)
            . ( transformOn biplate $ \mgi -> case mgi of
                  MGIUDPInst t s d i r l a -> MGIUDPInst (nmap t) s d i r l a
                  MGIModInst t p i r a -> MGIModInst (nmap t) p i r a
                  MGIUnknownInst t p i r a0 a -> MGIUnknownInst (nmap t) p i r a0 a
                  _ -> mgi
              )
            . (biplate . hierIdentFirst %~ nmap)
        )
        m
    )
    (po & each . poIdent . hierIdentFirst %~ nmap)
    (p & each . pbIdent %~ f)
    ( map
        ( (cbIdent %~ f)
            . (biplate %~ \(Dot1Ident l c) -> Dot1Ident l $ case l of Nothing -> nmap c; Just _ -> c)
            . (cbBody . each . ciCell_inst . _Right %~ \(h :| t) -> nmap h :| t)
        )
        c
    )
  where
    nmap x = HashMap.findWithDefault x x mapmap
    mapmap =
      HashMap.fromList $
        map (\x -> (x, f x)) $
          m ^.. each . mbIdent ++ p ^.. each . pbIdent ++ c ^.. each . cbIdent
