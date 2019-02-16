{-|
Module      : VeriFuzz.Lexer
Description : Lexer for Verilog.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Lexer for Verilog.
-}

module VeriFuzz.Lexer
  ( lexer
  , identifier
  , reserved
  , operator
  , reservedOp
  , charLiteral
  , stringLiteral
  , natural
  , integer
  , float
  , naturalOrFloat
  , decimal
  , hexadecimal
  , octal
  , symbol
  , lexeme
  , whiteSpace
  , parens
  , braces
  , angles
  , brackets
  , squares
  , comma
  , colon
  , dot
  , semiSep
  , semiSep1
  , commaSep
  , commaSep1
  ) where

import           Text.Parsec
import qualified Text.Parsec.Token as P

type VerilogDef = P.LanguageDef ()

type Lexer = P.TokenParser ()

type Parser = Parsec String ()

verilogDef :: VerilogDef
verilogDef = P.LanguageDef "/*" "*/" "//" False letter (alphaNum <|> char '_')
             (oneOf ":!#$%&*+./<=>?@\\^|-~") (oneOf ":!#$%&*+./<=>?@\\^|-~")
             reserved' reservedOp' True

lexer :: Lexer
lexer = P.makeTokenParser verilogDef

identifier :: Parser String
identifier = P.identifier lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer

operator :: Parser String
operator = P.operator lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

charLiteral :: Parser Char
charLiteral = P.charLiteral lexer

stringLiteral :: Parser String
stringLiteral = P.stringLiteral lexer

natural :: Parser Integer
natural = P.natural lexer

integer :: Parser Integer
integer = P.integer lexer

float :: Parser Double
float = P.float lexer

naturalOrFloat :: Parser (Either Integer Double)
naturalOrFloat = P.naturalOrFloat lexer

decimal :: Parser Integer
decimal = P.decimal lexer

hexadecimal :: Parser Integer
hexadecimal = P.hexadecimal lexer

octal :: Parser Integer
octal = P.octal lexer

symbol :: String -> Parser String
symbol = P.symbol lexer

lexeme :: Parser a -> Parser a
lexeme = P.lexeme lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

parens :: Parser a -> Parser a
parens = P.parens lexer

braces :: Parser a -> Parser a
braces = P.braces lexer

angles :: Parser a -> Parser a
angles = P.angles lexer

brackets :: Parser a -> Parser a
brackets = P.brackets lexer

squares :: Parser a -> Parser a
squares = P.squares lexer

comma :: Parser String
comma = P.comma lexer

colon :: Parser String
colon = P.colon lexer

dot :: Parser String
dot = P.dot lexer

semiSep :: Parser a -> Parser [a]
semiSep = P.semiSep lexer

semiSep1 :: Parser a -> Parser [a]
semiSep1 = P.semiSep1 lexer

commaSep :: Parser a -> Parser [a]
commaSep = P.commaSep lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = P.commaSep1 lexer

reservedOp' :: [String]
reservedOp' = [ "!", "~", "~&", "~|", "+", "-", "*", "/", "%", "==", "!=", "===", "!=="
              , "&&", "||", "<", "<=", ">", ">=", "&", "|", "^", "^~", "~^", "**", "<<"
              , ">>", "<<<", ">>>"
              ]

reserved' :: [String]
reserved' = [ "always", "and", "assign", "automatic", "begin", "buf", "bufif0", "bufif1"
            , "case", "casex", "casez", "cell", "cmos", "config", "deassign", "default"
            , "defparam", "design", "disable", "edge", "else", "end", "endcase", "endconfig"
            , "endfunction", "endgenerate", "endmodule", "endprimitive", "endspecify", "endtable"
            , "endtask", "event", "for", "force", "forever", "fork", "function", "generate", "genvar"
            , "highz0", "highz1", "if", "ifnone", "incdir", "include", "initial", "inout", "input"
            , "instance", "integer", "join", "large", "liblist", "library", "localparam", "macromodule"
            , "medium", "module", "nand", "negedge", "nmos", "nor", "noshowcancelled", "not", "notif0"
            , "notif1", "or", "output", "parameter", "pmos", "posedge", "primitive", "pull0", "pull1"
            , "pulldown", "pullup", "pulsestyle_onevent", "pulsestyle_ondetect", "remos", "real"
            , "realtime", "reg", "release", "repeat", "rnmos", "rpmos", "rtran", "rtranif0", "rtranif1"
            , "scalared", "showcancelled", "signed", "small", "specify", "specparam", "strong0", "strong1"
            , "supply0", "supply1", "table", "task", "time", "tran", "tranif0", "tranif1", "tri", "tri0"
            , "tri1", "triand", "trior", "trireg", "unsigned", "use", "vectored", "wait", "wand", "weak0"
            , "weak1", "while", "wire", "wor", "xnor", "xor"
            ]

