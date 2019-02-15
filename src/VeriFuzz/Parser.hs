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
import           Data.Attoparsec.Expr
import           Data.Attoparsec.Text as A
import           Data.Char            (isLetter)
import           Data.Functor         (($>))
import           Data.Text            (Text)
import qualified Data.Text            as T
import           VeriFuzz.AST
import           VeriFuzz.CodeGen

sBinOp :: BinaryOperator -> Expr -> Expr -> Expr
sBinOp = sOp BinOp
  where
    sOp f b a = f a b

parseExpr :: Parser Expr
parseExpr = buildExpressionParser parseTable parseTerm
            <?> "expr"

parseParens :: Parser a -> Parser a
parseParens a = do
  val <- "(" *> skipSpace *> a
  _ <- skipSpace *> ")"
  return val

constP :: Parser a -> Text -> Parser a
constP p t = case parseOnly p t of
  Left _  -> fail "constP"
  Right a -> return a

parseOf :: Parser Text -> Parser a -> Parser a
parseOf ptxt pa = bothParse
  where
    bothParse = ptxt >>= constP pa

ignoreWS :: Parser a -> Parser a
ignoreWS a = do
  skipSpace
  t <- a
  skipSpace
  return t

parseTerm :: Parser Expr
parseTerm = (Concat <$> aroundList "{" "}" parseExpr)
            <|> parseCond
            <|> parseParens parseExpr
            <|> ignoreWS (Number 32 <$> decimal)
            <?> "simple expr"

takeUntil :: Char -> Parser Text
takeUntil c = do
  t <- takeWhile1 (/=c)
  _ <- char c
  return t

parseCond :: Parser Expr
parseCond = do
  x <- parseOf (takeUntil '?') parseExpr
  y <- parseOf (takeUntil ':') parseExpr
  Cond x y <$> parseExpr

parseTable :: [[Operator Text Expr]]
parseTable =
  [ [ prefix "!" (UnOp UnLNot), prefix "~" (UnOp UnNot) ]
  , [ prefix "&" (UnOp UnAnd), prefix "|" (UnOp UnOr), prefix "~&" (UnOp UnNand)
    , prefix "~|" (UnOp UnNor), prefix "^" (UnOp UnXor), prefix "~^" (UnOp UnNxor)
    , prefix "^~" (UnOp UnNxorInv)
    ]
  , [ prefix "+" (UnOp UnPlus), prefix "-" (UnOp UnMinus) ]
  , [ binary "**" (sBinOp BinPower) AssocRight ]
  , [ binary "*" (sBinOp BinTimes) AssocLeft, binary "/" (sBinOp BinDiv) AssocLeft
    , binary "%" (sBinOp BinMod) AssocLeft
    ]
  , [ binary "+" (sBinOp BinPlus) AssocLeft, binary "-" (sBinOp BinPlus) AssocLeft ]
  , [ binary "<<" (sBinOp BinLSL) AssocLeft, binary ">>" (sBinOp BinLSR) AssocLeft ]
  , [ binary "<<<" (sBinOp BinASL) AssocLeft, binary ">>>" (sBinOp BinASR) AssocLeft ]
  , [ binary "<" (sBinOp BinLT) AssocNone, binary ">" (sBinOp BinGT) AssocNone
    , binary "<=" (sBinOp BinLEq) AssocNone, binary ">=" (sBinOp BinLEq) AssocNone
    ]
  , [ binary "==" (sBinOp BinEq) AssocNone, binary "!=" (sBinOp BinNEq) AssocNone ]
  , [ binary "===" (sBinOp BinEq) AssocNone, binary "!==" (sBinOp BinNEq) AssocNone ]
  , [ binary "&" (sBinOp BinAnd) AssocLeft ]
  , [ binary "^" (sBinOp BinXor) AssocLeft, binary "^~" (sBinOp BinXNor) AssocLeft
    , binary "~^" (sBinOp BinXNorInv) AssocLeft
    ]
  , [ binary "|" (sBinOp BinOr) AssocLeft ]
  , [ binary "&&" (sBinOp BinLAnd) AssocLeft ]
  , [ binary "|" (sBinOp BinLOr) AssocLeft ]
  ]

binary :: Text -> (a -> a -> a) -> Assoc -> Operator Text a
binary name fun = Infix ((string name <?> "binary") >> return fun)

prefix :: Text -> (a -> a) -> Operator Text a
prefix name fun = Prefix ((string name <?> "prefix") >> return fun)

postfix :: Text -> (a -> a) -> Operator Text a
postfix name fun = Postfix ((string name <?> "postfix") >> return fun)

commaSep :: Parser a -> Parser [a]
commaSep f = sepBy f (skipSpace *> char ',' *> skipSpace)

aroundList :: Parser a -> Parser b -> Parser c -> Parser [c]
aroundList a b c = do
  l <- a *> skipSpace *> commaSep c
  _ <- b
  return l

parseContAssign :: Parser ContAssign
parseContAssign = do
  var <- Identifier <$> (skipSpace *> "assign" *> skipSpace *> takeWhile1 isLetter)
  expr <- skipSpace *> "=" *> skipSpace *> parseExpr
  _ <- skipSpace *> ";"
  return $ ContAssign var expr

parseModItem :: Parser [ModItem]
parseModItem = fmap ModCA <$> many1 parseContAssign

parseModList :: Parser [Identifier]
parseModList = list <|> skipSpace $> []
  where
    list = fmap Identifier
           <$> aroundList "(" ")" (takeWhile1 isLetter)

parseModDecl :: Parser ModDecl
parseModDecl = do
  name <- Identifier <$> ("module" *> skipSpace *> takeWhile1 isLetter)
  modL <- fmap (Port Wire 1) <$> (skipSpace *> parseModList)
  _ <- skipSpace *> ";"
  modItem <- parseModItem <|> skipSpace $> []
  _ <- skipSpace *> "endmodule"
  return $ ModDecl name [Port Wire 1 "y"] modL modItem

parseDescription :: Parser Description
parseDescription = Description <$> (skipSpace *> parseModDecl)

parseVerilogSrc :: Parser VerilogSrc
parseVerilogSrc = VerilogSrc <$> many1 parseDescription
