{-|
Module      : VeriFuzz.Parser
Description : Minimal Verilog parser to reconstruct the AST.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Minimal Verilog parser to reconstruct the AST. This parser does not support the
whole Verilog syntax, as the AST does not support it either.
-}

module VeriFuzz.Parser where

import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text
import           Data.Functor         (($>))
import           Data.Text            (Text)
import qualified Data.Text            as T
import           VeriFuzz.AST
import           VeriFuzz.CodeGen

commaSep :: Parser a -> Parser [a]
commaSep f = sepBy f (skipSpace *> char ',' *> skipSpace)

aroundList :: Parser a -> Parser b -> Parser c -> Parser [c]
aroundList a b c = do
  l <- a *> skipSpace *> commaSep c
  _ <- b
  return l

parseBinOp :: Parser BinaryOperator
parseBinOp =
  "+"       $> BinPlus
  <|> "-"   $> BinMinus
  <|> "*"   $> BinTimes
  <|> "/"   $> BinDiv
  <|> "%"   $> BinMod
  <|> "=="  $> BinEq
  <|> "!="  $> BinNEq
  <|> "===" $> BinCEq
  <|> "!==" $> BinCNEq
  <|> "&&"  $> BinLAnd
  <|> "||"  $> BinLOr
  <|> "<"   $> BinLT
  <|> "<="  $> BinLEq
  <|> ">"   $> BinGT
  <|> ">="  $> BinGEq
  <|> "&"   $> BinAnd
  <|> "|"   $> BinOr
  <|> "^"   $> BinXor
  <|> "^~"  $> BinXNor
  <|> "~^"  $> BinXNorInv
  <|> "**"  $> BinPower
  <|> "<<"  $> BinLSL
  <|> ">>"  $> BinLSR
  <|> "<<<" $> BinASL
  <|> ">>>" $> BinASR

parseUnOp :: Parser UnaryOperator
parseUnOp =
  "+" $> UnPlus
  <|> "-" $> UnMinus
  <|> "!" $> UnLNot
  <|> "~" $> UnNot
  <|> "&" $> UnAnd
  <|> "~&" $> UnNand
  <|> "|" $> UnOr
  <|> "~|" $> UnNor
  <|> "^" $> UnXor
  <|> "~^" $> UnNxor
  <|> "^~" $> UnNxorInv

parseExpr :: Parser Expr
parseExpr = cond <|> binop <|> unop <|> conc <|> brack <|> var <|> num
  where
    var = Id . Identifier . T.pack <$> many1 letter
    num = Number 32 <$> decimal
    binop = do
      lhs <- var <|> num
      bo <- skipSpace *> parseBinOp
      skipSpace
      BinOp lhs bo <$> parseExpr
    brack = do
      expr <- "(" *> skipSpace *> parseExpr
      skipSpace *> ")" *> skipSpace
      return expr
    cond = do
      expr1 <- parseExpr
      skipSpace *> "?" *> skipSpace
      expr2 <- parseExpr
      skipSpace *> ":" *> skipSpace
      expr3 <- parseExpr
      skipSpace
      return $ Cond expr1 expr2 expr3
    conc = Concat <$> aroundList "{" "}" parseExpr
    unop = do
      uo <- parseUnOp
      skipSpace
      UnOp uo <$> parseExpr

parseContAssign :: Parser ContAssign
parseContAssign = do
  var <- Identifier . T.pack <$> (skipSpace *> "assign" *> skipSpace *> many1 letter)
  expr <- skipSpace *> "=" *> skipSpace *> parseExpr
  _ <- skipSpace *> ";"
  return $ ContAssign var expr

parseModItem :: Parser [ModItem]
parseModItem = fmap ModCA <$> many1 parseContAssign

parseModList :: Parser [Identifier]
parseModList = list <|> skipSpace $> []
  where
    list = fmap (Identifier . T.pack)
           <$> aroundList "(" ")" (many1 letter)

parseModDecl :: Parser ModDecl
parseModDecl = do
  name <- Identifier . T.pack <$> ("module" *> skipSpace *> many1 letter)
  modL <- fmap (Port Wire 1) <$> (skipSpace *> parseModList)
  _ <- skipSpace *> ";"
  modItem <- parseModItem <|> skipSpace $> []
  _ <- skipSpace *> "endmodule"
  return $ ModDecl name [Port Wire 1 "y"] modL modItem

parseDescription :: Parser Description
parseDescription = Description <$> (skipSpace *> parseModDecl)

parseVerilogSrc :: Parser VerilogSrc
parseVerilogSrc = VerilogSrc <$> many1 parseDescription
