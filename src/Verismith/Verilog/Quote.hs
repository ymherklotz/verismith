{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Verismith.Verilog.Quote
-- Description : QuasiQuotation for verilog code in Haskell.
-- Copyright   : (c) 2019, Yann Herklotz Grave
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- QuasiQuotation for verilog code in Haskell.
module Verismith.Verilog.Quote
  ( verilog,
  )
where

import Data.Data
import qualified Data.Text as T
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Verismith.Verilog.AST (Verilog)
import Verismith.Verilog.Parser

liftDataWithText :: Data a => a -> Q Exp
liftDataWithText = dataToExpQ $ fmap liftText . cast

liftText :: T.Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

-- | Quasiquoter for verilog, so that verilog can be written inline and be
-- parsed to an AST at compile time.
verilog :: QuasiQuoter
verilog =
  QuasiQuoter
    { quoteExp = quoteVerilog,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
    }

quoteVerilog :: String -> TH.Q TH.Exp
quoteVerilog s = do
  loc <- TH.location
  let pos = T.pack $ TH.loc_filename loc
  v <- case parseVerilog pos (T.pack s) of
    Right e -> return e
    Left e -> fail $ show e
  liftDataWithText (v :: Verilog ())
