-- |
-- Module      : Verismith.Verilog.Parser
-- Description : Minimal Verilog parser to reconstruct the AST.
-- Copyright   : (c) 2019-2022, Yann Herklotz
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- Minimal Verilog parser to reconstruct the AST. This parser does not support the
-- whole Verilog syntax, as the AST does not support it either.
module Verismith.Verilog.Parser
  ( -- * Parser
    parseVerilog,
    parseVerilogFile,
    parseSourceInfoFile,

    -- ** Internal parsers
    parseEvent,
    parseStatement,
    parseModItem,
    parseModDecl,
    Parser,
  )
where

import Control.Lens
import Control.Monad (void)
import Data.Bifunctor (bimap)
import Data.Bits
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Data.List (isInfixOf, isPrefixOf, null)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Parsec hiding (satisfy)
import Text.Parsec.Expr
import Verismith.Internal
import Verismith.Verilog.AST
import Verismith.Verilog.BitVec
import Verismith.Verilog.Internal
import Verismith.Verilog.Lex
import Verismith.Verilog.Preprocess
import Verismith.Verilog.Token

type Parser = Parsec [Token] ()

type ParseOperator = Operator [Token] () Identity

data Decimal = Decimal Int Integer

instance Num Decimal where
  (Decimal sa na) + (Decimal sb nb) = Decimal (max sa sb) (na + nb)
  (Decimal sa na) - (Decimal sb nb) = Decimal (max sa sb) (na - nb)
  (Decimal sa na) * (Decimal sb nb) = Decimal (max sa sb) (na * nb)
  negate (Decimal s n) = Decimal s $ negate n
  abs (Decimal s n) = Decimal s $ abs n
  signum (Decimal s n) = Decimal s $ signum n
  fromInteger = Decimal 32 . fromInteger

-- | This parser succeeds whenever the given predicate returns true when called
-- with parsed `Token`. Same as 'Text.Parsec.Char.satisfy'.
satisfy :: (Token -> Bool) -> Parser TokenName
satisfy f = tokenPrim show nextPos tokeq
  where
    tokeq :: Token -> Maybe TokenName
    tokeq t@(Token t' _ _) = if f t then Just t' else Nothing

satisfy' :: (Token -> Maybe a) -> Parser a
satisfy' = tokenPrim show nextPos

nextPos :: SourcePos -> Token -> [Token] -> SourcePos
nextPos pos _ (Token _ _ (Position _ l c) : _) =
  setSourceColumn (setSourceLine pos l) c
nextPos pos _ [] = pos

-- | Parses given `TokenName`.
tok :: TokenName -> Parser TokenName
tok t = satisfy (\(Token t' _ _) -> t' == t) <?> show t

-- | Parse without returning the `TokenName`.
tok' :: TokenName -> Parser ()
tok' p = void $ tok p

parens :: Parser a -> Parser a
parens = between (tok SymParenL) (tok SymParenR)

brackets :: Parser a -> Parser a
brackets = between (tok SymBrackL) (tok SymBrackR)

braces :: Parser a -> Parser a
braces = between (tok SymBraceL) (tok SymBraceR)

sBinOp :: BinaryOperator -> Expr -> Expr -> Expr
sBinOp = sOp BinOp where sOp f b a = f a b

parseExpr' :: Parser Expr
parseExpr' = buildExpressionParser parseTable parseTerm <?> "expr"

decToExpr :: Decimal -> Expr
decToExpr (Decimal s n) = Number $ bitVec s n

-- | Parse a Number depending on if it is in a hex or decimal form. Octal and
-- binary are not supported yet.
parseNum :: Parser Expr
parseNum = decToExpr <$> number

parseVar :: Parser Expr
parseVar = Id <$> identifier

parseVecSelect :: Parser Expr
parseVecSelect = do
  i <- identifier
  expr <- brackets parseExpr
  return $ VecSelect i expr

parseRangeSelect :: Parser Expr
parseRangeSelect = do
  i <- identifier
  range <- parseRange
  return $ RangeSelect i range

systemFunc :: Parser String
systemFunc = satisfy' matchId
  where
    matchId (Token IdSystem s _) = Just s
    matchId _ = Nothing

parseFun :: Parser Expr
parseFun = do
  f <- systemFunc
  expr <- parens parseExpr
  return $ Appl (Identifier $ T.pack f) expr

parserNonEmpty :: [a] -> Parser (NonEmpty a)
parserNonEmpty (a : b) = return $ a :| b
parserNonEmpty [] = fail "Concatenation cannot be empty."

parseTerm :: Parser Expr
parseTerm =
  parens parseExpr
    <|> (Concat <$> (braces (commaSep parseExpr) >>= parserNonEmpty))
    <|> parseFun
    <|> parseNum
    <|> try parseVecSelect
    <|> try parseRangeSelect
    <|> parseVar
    <?> "simple expr"

-- | Parses the ternary conditional operator. It will behave in a right
-- associative way.
parseCond :: Expr -> Parser Expr
parseCond e = do
  tok' SymQuestion
  expr <- parseExpr
  tok' SymColon
  Cond e expr <$> parseExpr

parseExpr :: Parser Expr
parseExpr = do
  e <- parseExpr'
  option e . try $ parseCond e

parseConstExpr :: Parser ConstExpr
parseConstExpr = fmap exprToConst parseExpr

-- | Table of binary and unary operators that encode the right precedence for
-- each.
parseTable :: [[ParseOperator Expr]]
parseTable =
  [ [prefix SymBang (UnOp UnLNot), prefix SymTildy (UnOp UnNot)],
    [ prefix SymAmp (UnOp UnAnd),
      prefix SymBar (UnOp UnOr),
      prefix SymTildyAmp (UnOp UnNand),
      prefix SymTildyBar (UnOp UnNor),
      prefix SymHat (UnOp UnXor),
      prefix SymTildyHat (UnOp UnNxor),
      prefix SymHatTildy (UnOp UnNxorInv)
    ],
    [prefix SymPlus (UnOp UnPlus), prefix SymDash (UnOp UnMinus)],
    [binary SymAsterAster (sBinOp BinPower) AssocRight],
    [ binary SymAster (sBinOp BinTimes) AssocLeft,
      binary SymSlash (sBinOp BinDiv) AssocLeft,
      binary SymPercent (sBinOp BinMod) AssocLeft
    ],
    [ binary SymPlus (sBinOp BinPlus) AssocLeft,
      binary SymDash (sBinOp BinMinus) AssocLeft
    ],
    [ binary SymLtLt (sBinOp BinLSL) AssocLeft,
      binary SymGtGt (sBinOp BinLSR) AssocLeft
    ],
    [ binary SymLtLtLt (sBinOp BinASL) AssocLeft,
      binary SymGtGtGt (sBinOp BinASR) AssocLeft
    ],
    [ binary SymLt (sBinOp BinLT) AssocNone,
      binary SymGt (sBinOp BinGT) AssocNone,
      binary SymLtEq (sBinOp BinLEq) AssocNone,
      binary SymGtEq (sBinOp BinGEq) AssocNone
    ],
    [ binary SymEqEq (sBinOp BinEq) AssocNone,
      binary SymBangEq (sBinOp BinNEq) AssocNone
    ],
    [ binary SymEqEqEq (sBinOp BinEq) AssocNone,
      binary SymBangEqEq (sBinOp BinNEq) AssocNone
    ],
    [binary SymAmp (sBinOp BinAnd) AssocLeft],
    [ binary SymHat (sBinOp BinXor) AssocLeft,
      binary SymHatTildy (sBinOp BinXNor) AssocLeft,
      binary SymTildyHat (sBinOp BinXNorInv) AssocLeft
    ],
    [binary SymBar (sBinOp BinOr) AssocLeft],
    [binary SymAmpAmp (sBinOp BinLAnd) AssocLeft],
    [binary SymBarBar (sBinOp BinLOr) AssocLeft]
  ]

binary :: TokenName -> (a -> a -> a) -> Assoc -> ParseOperator a
binary name fun = Infix ((tok name <?> "binary") >> return fun)

prefix :: TokenName -> (a -> a) -> ParseOperator a
prefix name fun = Prefix ((tok name <?> "prefix") >> return fun)

commaSep :: Parser a -> Parser [a]
commaSep = flip sepBy $ tok SymComma

toNE :: Parser [a] -> Parser (NonEmpty a)
toNE p = do
  p' <- p
  case p' of
    a : b -> return $ a :| b
    _ -> fail "List is empty."

commaSepNE :: Parser a -> Parser (NonEmpty a)
commaSepNE = toNE . commaSep

parseContAssign :: Parser ContAssign
parseContAssign = do
  var <- tok KWAssign *> identifier
  expr <- tok SymEq *> parseExpr
  tok' SymSemi
  return $ ContAssign var expr

numLit :: Parser String
numLit = satisfy' matchId
  where
    matchId (Token LitNumber s _) = Just s
    matchId _ = Nothing

number :: Parser Decimal
number = number' <$> numLit
  where
    number' :: String -> Decimal
    number' a
      | all (`elem` ['0' .. '9']) a = fromInteger $ read a
      | head a == '\'' = fromInteger $ f a
      | "'" `isInfixOf` a = Decimal (read w) (f b)
      | otherwise = error $ "Invalid number format: " ++ a
      where
        w = takeWhile (/= '\'') a
        b = dropWhile (/= '\'') a
        f a'
          | "'d" `isPrefixOf` a' = read $ drop 2 a'
          | "'h" `isPrefixOf` a' = read $ "0x" ++ drop 2 a'
          | "'b" `isPrefixOf` a' =
            foldl
              (\n b' -> shiftL n 1 .|. (if b' == '1' then 1 else 0))
              0
              (drop 2 a')
          | otherwise = error $ "Invalid number format: " ++ a'

-- toInteger' :: Decimal -> Integer
-- toInteger' (Decimal _ n) = n

toInt' :: Decimal -> Int
toInt' (Decimal _ n) = fromInteger n

-- | Parse a range and return the total size. As it is inclusive, 1 has to be
-- added to the difference.
parseRange :: Parser Range
parseRange = do
  rangeH <- tok SymBrackL *> parseConstExpr
  rangeL <- tok SymColon *> parseConstExpr
  tok' SymBrackR
  return $ Range rangeH rangeL

strId :: Parser String
strId = satisfy' matchId
  where
    matchId (Token IdSimple s _) = Just s
    matchId (Token IdEscaped s _) = Just s
    matchId _ = Nothing

identifier :: Parser Identifier
identifier = Identifier . T.pack <$> strId

parseNetDecl :: Maybe PortDir -> Parser (ModItem ann)
parseNetDecl pd = do
  t <- option Wire type_
  sign <- option False (tok KWSigned $> True)
  range <- option 1 parseRange
  name <- identifier
  i <- option Nothing (fmap Just (tok' SymEq *> parseConstExpr))
  tok' SymSemi
  return $ Decl pd (Port t sign range name) i
  where
    type_ = tok KWWire $> Wire <|> tok KWReg $> Reg

parsePortDir :: Parser PortDir
parsePortDir =
  tok KWOutput
    $> PortOut
    <|> tok KWInput
    $> PortIn
    <|> tok KWInout
    $> PortInOut

parseDecl :: Parser (ModItem ann)
parseDecl = (Just <$> parsePortDir >>= parseNetDecl) <|> parseNetDecl Nothing

parseConditional :: Parser (Statement ann)
parseConditional = do
  expr <- tok' KWIf *> parens parseExpr
  true <- maybeEmptyStatement
  false <- option Nothing (tok' KWElse *> maybeEmptyStatement)
  return $ CondStmnt expr true false

parseLVal :: Parser LVal
parseLVal = fmap RegConcat (braces $ commaSep parseExpr) <|> ident
  where
    ident = do
      i <- identifier
      (try (ex i) <|> try (sz i) <|> return (RegId i))
    ex i = do
      e <- tok' SymBrackL *> parseExpr
      tok' SymBrackR
      return $ RegExpr i e
    sz i = RegSize i <$> parseRange

parseDelay :: Parser Delay
parseDelay = Delay . toInt' <$> (tok' SymPound *> number)

parseAssign :: TokenName -> Parser Assign
parseAssign t = do
  lval <- parseLVal
  tok' t
  delay <- option Nothing (fmap Just parseDelay)
  expr <- parseExpr
  return $ Assign lval delay expr

parseLoop :: Parser (Statement ann)
parseLoop = do
  a <- tok' KWFor *> tok' SymParenL *> parseAssign SymEq
  expr <- tok' SymSemi *> parseExpr
  incr <- tok' SymSemi *> parseAssign SymEq
  tok' SymParenR
  statement <- parseStatement
  return $ ForLoop a expr incr statement

parseDefaultPair :: Parser (Statement a)
parseDefaultPair = tok' KWDefault *> tok' SymColon *> parseStatement

parseCasePair :: Parser (CasePair ann)
parseCasePair = do
  expr <- parseExpr <* tok' SymColon
  CasePair expr <$> parseStatement

parseCase :: Parser (Statement ann)
parseCase = do
  expr <- tok' KWCase *> parseExpr
  cp <- manyTill parseCasePair (lookAhead ((parseDefaultPair $> ()) <|> tok' KWEndcase))
  def <- option Nothing $ Just <$> parseDefaultPair
  tok' KWEndcase
  return (StmntCase CaseStandard expr cp def)

eventList :: TokenName -> Parser [Event]
eventList t = do
  l <- sepBy parseEvent' (tok t)
  if null l then fail "Could not parse list" else return l

parseEvent :: Parser Event
parseEvent =
  (tok' SymAtAster $> EAll)
    <|> try (tok' SymAt *> tok' SymParenLAsterParenR $> EAll)
    <|> try
      ( tok' SymAt
          *> parens (tok' SymAster)
          $> EAll
      )
    <|> try (tok' SymAt *> parens parseEvent')
    <|> try (tok' SymAt *> parens (foldr1 EOr <$> eventList KWOr))
    <|> try (tok' SymAt *> parens (foldr1 EComb <$> eventList SymComma))

parseEvent' :: Parser Event
parseEvent' =
  try (tok' KWPosedge *> fmap EPosEdge identifier)
    <|> try (tok' KWNegedge *> fmap ENegEdge identifier)
    <|> try (fmap EId identifier)
    <|> try (fmap EExpr parseExpr)

parseEventCtrl :: Parser (Statement ann)
parseEventCtrl = do
  event <- parseEvent
  statement <- option Nothing maybeEmptyStatement
  return $ EventCtrl event statement

parseDelayCtrl :: Parser (Statement ann)
parseDelayCtrl = do
  delay <- parseDelay
  statement <- option Nothing maybeEmptyStatement
  return $ TimeCtrl delay statement

parseBlocking :: Parser (Statement ann)
parseBlocking = do
  a <- parseAssign SymEq
  tok' SymSemi
  return $ BlockAssign a

parseNonBlocking :: Parser (Statement ann)
parseNonBlocking = do
  a <- parseAssign SymLtEq
  tok' SymSemi
  return $ NonBlockAssign a

parseSeq :: Parser (Statement ann)
parseSeq = do
  seq' <- tok' KWBegin *> many parseStatement
  tok' KWEnd
  return $ SeqBlock seq'

parseStatement :: Parser (Statement ann)
parseStatement =
  parseSeq
    <|> parseConditional
    <|> parseLoop
    <|> parseCase
    <|> parseEventCtrl
    <|> parseDelayCtrl
    <|> try parseBlocking
    <|> parseNonBlocking

maybeEmptyStatement :: Parser (Maybe (Statement ann))
maybeEmptyStatement =
  (tok' SymSemi >> return Nothing) <|> (Just <$> parseStatement)

parseAlways :: Parser (ModItem ann)
parseAlways = tok' KWAlways *> (Always <$> parseStatement)

parseInitial :: Parser (ModItem ann)
parseInitial = tok' KWInitial *> (Initial <$> parseStatement)

namedModConn :: Parser ModConn
namedModConn = do
  target <- tok' SymDot *> identifier
  expr <- parens parseExpr
  return $ ModConnNamed target expr

parseModConn :: Parser ModConn
parseModConn = try (fmap ModConn parseExpr) <|> namedModConn

parseModInst :: Parser (ModItem ann)
parseModInst = do
  m <- identifier
  params <- option [] $ tok' SymPound *> parens (commaSep parseModConn)
  name <- identifier
  modconns <- parens (commaSep parseModConn)
  tok' SymSemi
  return $ ModInst m params name modconns

parseParam :: Parser Parameter
parseParam = do
  i <- tok' KWParameter *> identifier
  expr <- tok' SymEq *> parseConstExpr
  return $ Parameter i expr

parseParam' :: Parser Parameter
parseParam' = do
  i <- identifier
  expr <- tok' SymEq *> parseConstExpr
  return $ Parameter i expr

parseParams :: Parser [Parameter]
parseParams = tok' SymPound *> parens (commaSep parseParam)

parseParamDecl :: Parser (ModItem ann)
parseParamDecl =
  ParamDecl <$> (tok' KWParameter *> commaSepNE parseParam' <* tok' SymSemi)

parseModItem :: Parser (ModItem ann)
parseModItem =
  try (ModCA <$> parseContAssign)
    <|> try parseDecl
    <|> parseAlways
    <|> parseInitial
    <|> parseModInst
    <|> parseParamDecl

parseModList :: Parser [Identifier]
parseModList = list <|> return [] where list = parens $ commaSep identifier

filterDecl :: PortDir -> (ModItem ann) -> Bool
filterDecl p (Decl (Just p') _ _) = p == p'
filterDecl _ _ = False

modPorts :: PortDir -> [ModItem ann] -> [Port]
modPorts p mis = filter (filterDecl p) mis ^.. traverse . declPort

parseModDecl :: Parser (ModDecl ann)
parseModDecl = do
  name <- tok KWModule *> identifier
  paramList <- option [] $ try parseParams
  _ <- fmap defaultPort <$> parseModList
  tok' SymSemi
  modItem <- option [] . try $ many1 parseModItem
  tok' KWEndmodule
  return $
    ModDecl
      name
      (modPorts PortOut modItem)
      (modPorts PortIn modItem)
      modItem
      paramList

mergeMaybe :: Maybe a -> Maybe a -> Maybe a
mergeMaybe (Just a) Nothing = Just a
mergeMaybe Nothing (Just a) = Just a
mergeMaybe a _ = a

mergeType :: PortType -> PortType -> PortType
mergeType Reg Wire = Reg
mergeType Wire Reg = Reg
mergeType a _ = a

mergePorts :: Port -> Port -> Port
mergePorts (Port t1 s1 r1 n1) (Port t2 s2 r2 n2) =
  Port (mergeType t1 t2) (s1 || s2) (if r1 == 0 then r2 else r1) n1

mergeIO :: ModItem a -> ModItem a -> ModItem a
mergeIO (Decl a1 b1 c1) (Decl a2 b2 c2) = Decl (mergeMaybe a1 a2) (mergePorts b1 b2) (mergeMaybe c1 c2)
mergeIO a _ = a

genmoditem :: Map.Map Identifier (ModItem a) -> ModItem a -> Map.Map Identifier (ModItem a)
genmoditem m (Decl a b c) =
  Map.insertWith mergeIO (b^.portName) (Decl a b c) m
genmoditem m b = m

modifyelements :: [ModItem a] -> [ModItem a]
modifyelements ma = ndecl <> nodecl
  where
    ndecl = Map.elems $ foldl genmoditem Map.empty ma
    isDecl Decl{} = True
    isDecl _ = False
    nodecl = filter isDecl ma

-- | Parses a 'String' into 'Verilog' by skipping any beginning whitespace
-- and then parsing multiple Verilog source.
parseVerilogSrc :: Parser (Verilog ann)
parseVerilogSrc = Verilog <$> many parseModDecl

-- | Parse a 'String' containing verilog code. The parser currently only supports
-- the subset of Verilog that is being generated randomly.
parseVerilog ::
  -- | Name of parsed object.
  Text ->
  -- | Content to be parsed.
  Text ->
  -- | Returns 'String' with error
  -- message if parse fails.
  Either Text (Verilog ann)
parseVerilog s =
  bimap showT id --(_Wrapped.traverse.modItems %~ modifyelements)
    . parse parseVerilogSrc (T.unpack s)
    . alexScanTokens
    . preprocess [] (T.unpack s)
    . T.unpack

parseVerilogFile :: Text -> IO (Verilog ann)
parseVerilogFile file = do
  src <- T.readFile $ T.unpack file
  case parseVerilog file src of
    Left s -> error $ T.unpack s
    Right r -> return r

parseSourceInfoFile :: Text -> Text -> IO (SourceInfo ann)
parseSourceInfoFile top = fmap (SourceInfo top) . parseVerilogFile
