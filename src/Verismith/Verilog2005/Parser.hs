-- Module      : Verismith.Verilog2005.Parser
-- Description : Partial Verilog 2005 parser to reconstruct the AST.
-- Copyright   : (c) 2023 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : experimental
-- Portability : POSIX

-- TODO: track paren/brace/brack L outside of enclosed
module Verismith.Verilog2005.Parser
  ( parseVerilog2005,
  )
where

import Control.Applicative (liftA2, liftA3)
import Control.Monad (join)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.CPS
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w)
import qualified Data.ByteString.Lazy as LB
import Data.Data (Data, constrIndex, toConstr)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.IntMap.Strict as IntMap
import Data.List
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import qualified Data.Vector.Unboxed as V
import Text.Parsec hiding (satisfy, uncons)
import Text.Parsec.Error
import Text.Parsec.Expr
import Text.Printf (printf)
import Verismith.Utils
import Verismith.Verilog2005.AST
import Verismith.Verilog2005.Lexer
import Verismith.Verilog2005.PrettyPrinter
import Verismith.Verilog2005.Token
import Verismith.Verilog2005.Utils

ptrace :: String -> Parser ()
ptrace s = do
  p <- getPosition
  inp <- getInput
  setInput $ take 20 inp
  parserTrace $ printf "at %d:%d, %s" (sourceLine p) (sourceColumn p) s
  setInput inp

type Parser = ParsecT [PosToken] LocalCompDir (Writer [String])

type Produce a = (Token, a)

type LProduce a = [Produce a]

type Branch a = Produce (Parser a)

type LBranch a = [Branch a]

type AProduce a = Produce ([Attribute] -> a)

type LAProduce a = [AProduce a]

type ABranch a = AProduce (Parser a)

type LABranch a = [ABranch a]

-- | An error that is not merged with other errors and expected tokens
hardfail :: String -> Parser a
hardfail m =
  mkPT $ \s -> return $ Consumed $ return $ Error $ newErrorMessage (Message m) (statePos s)

-- | Warning formatting
warn :: SourcePos -> String -> Parser ()
warn pos s = lift $ tell [printf "Line %d, column %d: %s" (sourceLine pos) (sourceColumn pos) s]

-- | Efficient token branching utility
getConsIndex :: Data a => a -> Int
getConsIndex = constrIndex . toConstr

mkActionMap :: LProduce a -> IntMap.IntMap a
mkActionMap =
  IntMap.fromListWithKey (\k _ _ -> error $ "Conflict on " ++ show k)
    . map (\(d, a) -> (getConsIndex d, a))

-- | Parse exactly one token and produce a value
nextPos :: SourcePos -> PosToken -> [PosToken] -> SourcePos
nextPos pos _ ptl = case ptl of
  PosToken (Somewhere l c) _ : _ -> setSourceColumn (setSourceLine pos l) c
  _ -> pos

producePrim :: (Token -> Maybe a) -> Parser a
producePrim f = tokenPrim show nextPos (f . _PTToken)

branchPrim :: (Token -> Maybe (Parser a)) -> Parser a
branchPrim = join . producePrim

-- | Parse these annoying compiler directives that can appear anywhere
anywherecompdir :: Parser ()
anywherecompdir = skipMany $
  branchPrim $ \t -> case t of
    CDLine -> Just $ do
      producePrim $ \t -> case t of LitDecimal _ -> Just (); _ -> Nothing
      producePrim $ \t -> case t of LitString _ -> Just (); _ -> Nothing
      producePrim $ \t -> case t of
        LitDecimal 0 -> Just ()
        LitDecimal 1 -> Just ()
        LitDecimal 2 -> Just ()
        _ -> Nothing
    CDCelldefine -> Just $ modifyState $ \s -> s {_LCDCell = True}
    CDEndcelldefine -> Just $ modifyState $ \s -> s {_LCDCell = False}
    _ -> Nothing

-- | Basic one token parsing able to produce a value
fproduce :: (Token -> Maybe a) -> Parser a
fproduce f = producePrim f <* anywherecompdir

lproduce :: LProduce a -> Parser a
lproduce l =
  fproduce (\t -> IntMap.lookup (getConsIndex t) $ mkActionMap l)
    `labels` map (\(d, _) -> show d) l

maplproduce :: (a -> b) -> LProduce a -> LProduce b
maplproduce f = map $ \(t, x) -> (t, f x)

maplaproduce :: (a -> b) -> LAProduce a -> LAProduce b
maplaproduce f = map $ \(t, p) -> (t, \a -> f $ p a)

-- | Try to consume the provided token
consume :: Token -> Parser ()
consume et = fproduce (\at -> if at == et then Just () else Nothing) <?> show et

optConsume :: Token -> Parser Bool
optConsume et = option False $ fproduce $ \at -> if at == et then Just True else Nothing

-- | Branch on the next token
fbranch :: (Token -> Maybe (Parser a)) -> Parser a
fbranch = join . fproduce

labranch :: LABranch a -> Parser a
labranch l = attributes >>= \a -> lproduce l >>= \p -> p a

mapabranch :: (a -> b) -> ABranch a -> ABranch b
mapabranch f (t, p) = (t, \a -> f <$> p a)

maplabranch :: (a -> b) -> LABranch a -> LABranch b
maplabranch = map . mapabranch

-- | Specialised repeating combinators
accrmany :: (a -> m a -> m a) -> m a -> Parser a -> Parser (m a)
accrmany f l p = loop l
  where
    loop acc = option acc $ p >>= \x -> loop $ f x acc

monoAccum :: Monoid a => Parser a -> Parser a
monoAccum p = loop mempty
  where
    loop acc = option acc $ p >>= \x -> loop $ x <> acc

-- | Econlosing that keeps track of the opening token position
enclosed :: Token -> Token -> Parser a -> Parser a
enclosed l r x =
  getPosition >>= \p ->
    consume l *> x
      <* ( fproduce (\t -> if t == r then Just () else Nothing)
             <?> printf
               "closing %s to match opening %s at %d:%d"
               (show r)
               (show l)
               (sourceLine p)
               (sourceColumn p)
         )

-- | Enclosed between parentheses/brackets/braces
parens :: Parser a -> Parser a
parens = enclosed SymParenL SymParenR

brackets :: Parser a -> Parser a
brackets = enclosed SymBrackL SymBrackR

braces :: Parser a -> Parser a
braces = enclosed SymBraceL SymBraceR

-- | Comma separated list with at least n (0) elements
csl :: Parser a -> Parser [a]
csl p = sepBy p $ consume SymComma

csl1 :: Parser a -> Parser (NonEmpty a)
csl1 p = NE.fromList <$> sepBy1 p (consume SymComma)

wcsl1 :: String -> Parser a -> Parser [a]
wcsl1 s p = do
  pos <- getPosition
  l <- csl p
  if null l then warn pos $ printf "Zero %s is a SystemVerilog feature" s else return ()
  return l

rcsl :: Parser a -> Parser [a]
rcsl p = option [] $ p >>= \x -> accrmany (:) [x] (consume SymComma *> p)

rcsl1 :: Parser a -> Parser (NonEmpty a)
rcsl1 p = p >>= \x -> accrmany (<|) (x :| []) (consume SymComma *> p)

wrcsl1 :: String -> Parser a -> Parser [a]
wrcsl1 s p = do
  pos <- getPosition
  l <- rcsl p
  if null l then warn pos $ printf "Zero %s is a SystemVerilog feature" s else return ()
  return l

-- | Safe parsing reversed comma separated list with at least 1 elements
srcsl1 :: Bool -> Parser a -> (a -> Parser b) -> Parser (NonEmpty b)
srcsl1 safety d p =
  d >>= p
    >>= \h -> accrmany (<|) (h :| []) (((if safety then try else id) $ consume SymComma *> d) >>= p)

-- | Parenthesised comma separated list
pcsl :: Parser a -> Parser [a]
pcsl = parens . csl

pcsl1 :: Parser a -> Parser (NonEmpty a)
pcsl1 = parens . csl1

bcsl1 :: Parser a -> Parser (NonEmpty a)
bcsl1 = braces . csl1

-- | Extracts an identifier
ident :: Parser B.ByteString
ident =
  fproduce (\t -> case t of IdSimple s -> Just s; IdEscaped s -> Just s; _ -> Nothing)
    <?> "identifier"

lenientIdent :: Parser B.ByteString
lenientIdent =
  getPosition >>= \pos ->
    fbranch
      ( \t -> case t of
          IdSimple s -> Just $ return s
          IdEscaped s -> Just $ return s
          IdSystem s -> Just $ do
            warn pos "Dollar prefixed identifier outside system function or task is not correct Verilog"
            return s
          _ -> Nothing
      )
      <?> "identifier"

-- | Dotted identifier with at most 1 dot
dot1Ident :: Parser Dot1Ident
dot1Ident = liftA2 Dot1Ident ident $ optionMaybe $ consume SymDot *> ident

-- | Attribute list
attribute :: Parser [Attribute]
attribute =
  enclosed SymParenAster SymAsterParen $
    NE.toList <$> csl1 (liftA2 Identified ident $ optionMaybe $ consume SymEq *> constExpr)

-- | Flattened list of attributes
attributes :: Parser [Attribute]
attributes = concat <$> many attribute

-- | Number after base
number :: Base -> Parser Number
number b = case b of
  BBin -> NBinary <$> fproduce (\t -> case t of LitBinary b -> Just $ NE.fromList b; _ -> Nothing)
  BOct -> NOctal <$> fproduce (\t -> case t of LitOctal o -> Just $ NE.fromList o; _ -> Nothing)
  BDec -> fproduce $ \t -> case t of
    LitXZ b -> Just $ NXZ b
    LitDecimal i -> Just $ NDecimal i
    _ -> Nothing
  BHex -> NHex <$> fproduce (\t -> case t of LitHex h -> Just $ NE.fromList h; _ -> Nothing)

type PGenExpr g i r =
  (B.ByteString -> Parser i) -> Parser r -> (Expr -> Maybe (GenExpr i r)) -> Parser (g i r)

-- | Parametric primary expression
genPrim :: PGenExpr GenPrim i r
genPrim pi pr constf = fbranch $ \t -> case t of
  -- try parse braceL and let that decide the path, otherwise it is wrong for constExpr
  SymBraceL -> Just $ do
    e <- expr
    b <- optConsume SymBraceL
    ee <-
      if b
        then case constifyExpr e of
          Nothing -> hardfail "Replication takes a constant expression as multiplicity"
          Just e -> PrimMultConcat e <$> csl1 (genExpr pi pr constf) <* consume SymBraceR
        else case constf e of
          Nothing -> hardfail "Invalid kind of expression"
          Just e -> PrimConcat . (e :|) <$> option [] (consume SymComma *> csl (genExpr pi pr constf))
    consume SymBraceR
    return ee
  SymParenL -> Just $ PrimMinTypMax <$> mtm (genExpr pi pr constf) <* consume SymParenR
  LitDecimal i -> Just $
    option (PrimNumber Nothing True $ NDecimal i) $
      fbranch $ \t -> case t of
        NumberBase s b -> Just $ PrimNumber (Just i) s <$> number b
        _ -> Nothing
  LitReal s -> Just $ return $ PrimReal s
  NumberBase s b -> Just $ PrimNumber Nothing s <$> number b
  LitString s -> Just $ return $ PrimString s
  IdSystem s ->
    Just $
      PrimSysFun s <$> option [] (parens $ wcsl1 "system function argument" $ genExpr pi pr constf)
  IdSimple s -> Just $ idp s
  IdEscaped s -> Just $ idp s
  _ -> Nothing
  where
    idp s =
      pi s >>= \ss ->
        liftA2 (PrimFun ss) attributes (parens $ wcsl1 "function argument" $ genExpr pi pr constf)
          <|> PrimIdent ss <$> pr

-- | Unary operator can only be applied on primary expressions
genBase :: PGenExpr GenExpr i r
genBase pi pr constf =
  option
    ExprPrim
    ( liftA2
        ExprUnOp
        ( fproduce $ \t -> case t of
            UnTilde -> Just UnNot
            UnBang -> Just UnLNot
            UnTildeAmp -> Just UnNand
            UnTildeBar -> Just UnNor
            AmBar -> Just UnOr
            AmHat -> Just UnXor
            AmAmp -> Just UnAnd
            AmTildeHat -> Just UnXNor
            SymPlus -> Just UnPlus
            SymDash -> Just UnMinus
            _ -> Nothing
        )
        attributes
    )
    <*> genPrim pi pr constf

-- | Facility for expression parsing
genExprBuildParser :: PGenExpr GenExpr i r
genExprBuildParser pi pr constf =
  buildExpressionParser
    [ infixop $ \t -> case t of BinAsterAster -> Just BinPower; _ -> Nothing,
      infixop $ \t -> case t of
        SymAster -> Just BinTimes
        BinSlash -> Just BinDiv
        BinPercent -> Just BinMod
        _ -> Nothing,
      infixop $ \t -> case t of SymPlus -> Just BinPlus; SymDash -> Just BinMinus; _ -> Nothing,
      infixop $ \t -> case t of
        BinLtLt -> Just BinLSL
        BinGtGt -> Just BinLSR
        BinLtLtLt -> Just BinASL
        BinGtGtGt -> Just BinASR
        _ -> Nothing,
      infixop $ \t -> case t of
        BinLt -> Just BinLT
        SymLtEq -> Just BinLEq
        BinGt -> Just BinGT
        BinGtEq -> Just BinGEq
        _ -> Nothing,
      infixop $ \t -> case t of
        BinEqEq -> Just BinEq
        BinBangEq -> Just BinNEq
        BinEqEqEq -> Just BinCEq
        BinBangEqEq -> Just BinCNEq
        _ -> Nothing,
      infixop $ \t -> case t of AmAmp -> Just BinAnd; _ -> Nothing,
      infixop $ \t -> case t of AmHat -> Just BinXor; AmTildeHat -> Just BinXNor; _ -> Nothing,
      infixop $ \t -> case t of AmBar -> Just BinOr; _ -> Nothing,
      infixop $ \t -> case t of BinAmpAmp -> Just BinLAnd; _ -> Nothing,
      infixop $ \t -> case t of BinBarBar -> Just BinLOr; _ -> Nothing
    ]
    (genBase pi pr constf)
  where
    infixop fp = [Infix (liftA2 (\op a l -> ExprBinOp l op a) (fproduce fp) attributes) AssocLeft]

-- | Parametric expression
genExpr :: PGenExpr GenExpr i r
genExpr pi pr constf =
  genExprBuildParser pi pr constf >>= \e ->
    option e $
      liftA3 (ExprCond e) (consume SymQuestion *> attributes) (genExpr pi pr constf) $
        consume SymColon *> genExpr pi pr constf

expr :: Parser Expr
expr = Expr <$> genExpr (trHierIdent True) dimRange (\(Expr e) -> Just e)

constExpr :: Parser CExpr
constExpr =
  CExpr
    <$> genExpr return (optionMaybe constRangeExpr) (fmap (\(CExpr e) -> e) . constifyExpr)

-- | Minimum, Typical, Maximum on a base type recognised by the argument parser
mtm :: Parser a -> Parser (GenMinTypMax a)
mtm p =
  p >>= \x ->
    option (MTMSingle x) $
      liftA2 (MTMFull x) (consume SymColon *> p) $ consume SymColon *> p

-- | Ranges
range2 :: Parser Range2
range2 = brackets $ liftA2 Range2 constExpr $ consume SymColon *> constExpr

genRangeExpr :: Parser e -> (e -> Maybe CExpr) -> Parser (GenRangeExpr e)
genRangeExpr pe constf =
  brackets $
    pe >>= \b ->
      option
        (GRESingle b)
        ( fproduce
            ( \t -> case t of
                SymColon -> (\m -> GREPair . Range2 m) <$> constf b
                SymPlusColon -> Just $ GREBaseOff b False
                SymDashColon -> Just $ GREBaseOff b True
                _ -> Nothing
            )
            <*> constExpr
        )

constRangeExpr :: Parser (CRangeExpr)
constRangeExpr = genRangeExpr constExpr Just

-- | Specify terminal
specTerm :: Parser SpecTerm
specTerm = liftA2 Identified ident $ optionMaybe constRangeExpr

-- | Reference and constant minimum typical maximum
cmtmRef :: Parser (Identified (Maybe CMinTypMax))
cmtmRef = option defaultIdM $ liftA2 Identified ident $ optionMaybe $ brackets $ mtm constExpr

-- | Sized reference
sz2Ref :: Parser (B.ByteString, Maybe Range2)
sz2Ref = option ("", Nothing) $ mkpair ident $ optionMaybe range2

-- | Signedness and range, both optional
signRange :: Parser SignRange
signRange = liftA2 SignRange (optConsume KWSigned) $ optionMaybe range2

-- | Index for each dimension then bit range
genDimRange :: Parser e -> (e -> Maybe CExpr) -> Parser (Maybe (GenDimRange e))
genDimRange pe constf =
  accrmany (:) [] (genRangeExpr pe constf) >>= \l -> case l of
    [] -> return Nothing
    h : t -> case rmapM (\re -> case re of GRESingle e -> Just e; _ -> Nothing) t of
      Nothing -> hardfail "Only the last bracketed expression is allowed to be a range"
      Just it -> return $ Just $ GenDimRange it h

dimRange :: Parser (Maybe DimRange)
dimRange = (fmap DimRange) <$> genDimRange expr constifyExpr

constDimRange :: Parser (Maybe CDimRange)
constDimRange = (fmap CDimRange) <$> genDimRange constExpr Just

-- | Hierarchical identifier
trHierIdent :: Bool -> B.ByteString -> Parser HierIdent
trHierIdent safety s = do
  l <-
    many $
      mkpair
        ((if safety then try else id) $ optionMaybe (brackets constExpr) <* consume SymDot)
        ident
  let hi =
        foldl'
          (\hi (index, ss) -> HierIdent (Identified (_HIIdent hi) index : _HIPath hi) ss)
          (HierIdent [] s)
          l
  return hi {_HIPath = reverse $ _HIPath hi}

hierIdent :: Bool -> Parser HierIdent
hierIdent safety = ident >>= trHierIdent safety

-- | Lvalues
lval :: Parser (Maybe dr) -> Parser (LValue dr)
lval p = LVConcat <$> bcsl1 (lval p) <|> liftA2 LVSingle (hierIdent True) p

netLV :: Parser NetLValue
netLV = lval constDimRange

varLV :: Parser VarLValue
varLV = lval dimRange

-- | Assignments
varAssign :: Parser VarAssign
varAssign = liftA2 VarAssign varLV $ consume SymEq *> expr

-- | converts HierIdent into ByteString
constifyIdent :: HierIdent -> Maybe B.ByteString
constifyIdent (HierIdent p i) = case p of [] -> Just i; _ -> Nothing

unconstIdent :: B.ByteString -> HierIdent
unconstIdent = HierIdent []

-- | converts Prim into GenPrim i r
constifyGenPrim ::
  (HierIdent -> Maybe i) ->
  (Maybe DimRange -> Maybe r) ->
  GenPrim HierIdent (Maybe DimRange) ->
  Maybe (GenPrim i r)
constifyGenPrim fi fr x = case x of
  PrimNumber s b n -> Just $ PrimNumber s b n
  PrimReal s -> Just $ PrimReal s
  PrimIdent s rng -> liftA2 PrimIdent (fi s) (fr rng)
  PrimConcat e -> PrimConcat <$> mapM ce e
  PrimMultConcat m e -> PrimMultConcat m <$> mapM ce e
  PrimFun s a e -> liftA2 (flip PrimFun a) (fi s) (mapM ce e)
  PrimSysFun s e -> PrimSysFun s <$> mapM ce e
  PrimMinTypMax (MTMSingle e) -> PrimMinTypMax . MTMSingle <$> ce e
  PrimMinTypMax (MTMFull l t h) -> PrimMinTypMax <$> liftA3 MTMFull (ce l) (ce t) (ce h)
  PrimString s -> Just $ PrimString s
  where
    ce = trConstifyGenExpr fi fr

unconstPrim :: GenPrim B.ByteString (Maybe CRangeExpr) -> GenPrim HierIdent (Maybe DimRange)
unconstPrim x = case x of
  PrimNumber s b n -> PrimNumber s b n
  PrimReal s -> PrimReal s
  PrimIdent s rng -> PrimIdent (unconstIdent s) (DimRange . GenDimRange [] . unconstRange <$> rng)
  PrimConcat e -> PrimConcat (NE.map trUnconstExpr e)
  PrimMultConcat m e -> PrimMultConcat m (NE.map trUnconstExpr e)
  PrimFun s a e -> PrimFun (unconstIdent s) a (map trUnconstExpr e)
  PrimSysFun s e -> PrimSysFun s (map trUnconstExpr e)
  PrimMinTypMax (MTMSingle e) -> PrimMinTypMax (MTMSingle (trUnconstExpr e))
  PrimMinTypMax (MTMFull l t h) ->
    PrimMinTypMax $ MTMFull (trUnconstExpr l) (trUnconstExpr t) (trUnconstExpr h)
  PrimString s -> PrimString s

-- | converts Expr into GenExpr i r
trConstifyGenExpr ::
  (HierIdent -> Maybe i) ->
  (Maybe DimRange -> Maybe r) ->
  GenExpr HierIdent (Maybe DimRange) ->
  Maybe (GenExpr i r)
trConstifyGenExpr fi fr x = case x of
  ExprPrim p -> ExprPrim <$> constifyGenPrim fi fr p
  ExprUnOp op a p -> ExprUnOp op a <$> constifyGenPrim fi fr p
  ExprBinOp lhs op a rhs -> liftA2 (\clhs -> ExprBinOp clhs op a) (ce lhs) (ce rhs)
  ExprCond c a t f -> liftA3 (flip ExprCond a) (ce c) (ce t) (ce f)
  where
    ce = trConstifyGenExpr fi fr

trConstifyExpr ::
  GenExpr HierIdent (Maybe DimRange) -> Maybe (GenExpr B.ByteString (Maybe CRangeExpr))
trConstifyExpr = trConstifyGenExpr constifyIdent $
  maybe (Just Nothing) $
    \(DimRange (GenDimRange l r)) -> if null l then Just <$> constifyRange r else Nothing

constifyExpr :: Expr -> Maybe CExpr
constifyExpr (Expr e) = CExpr <$> trConstifyExpr e

trUnconstExpr :: GenExpr B.ByteString (Maybe CRangeExpr) -> GenExpr HierIdent (Maybe DimRange)
trUnconstExpr x = case x of
  ExprPrim p -> ExprPrim (unconstPrim p)
  ExprUnOp op a p -> ExprUnOp op a (unconstPrim p)
  ExprBinOp lhs op a rhs -> ExprBinOp (trUnconstExpr lhs) op a (trUnconstExpr rhs)
  ExprCond c a t f -> ExprCond (trUnconstExpr c) a (trUnconstExpr t) (trUnconstExpr f)

unconstExpr :: CExpr -> Expr
unconstExpr (CExpr e) = Expr (trUnconstExpr e)

-- | converts RangeExpr into CRangeExpr
constifyRange :: RangeExpr -> Maybe CRangeExpr
constifyRange x = case x of
  GRESingle e -> GRESingle <$> constifyExpr e
  GREPair r2 -> Just $ GREPair r2
  GREBaseOff b mp o -> (\cb -> GREBaseOff cb mp o) <$> constifyExpr b

unconstRange :: CRangeExpr -> RangeExpr
unconstRange x = case x of
  GRESingle e -> GRESingle (unconstExpr e)
  GREPair r2 -> GREPair r2
  GREBaseOff b mp o -> GREBaseOff (unconstExpr b) mp o

-- | converts DimRange into CDimRange
trConstifyDR :: GenDimRange Expr -> Maybe (GenDimRange CExpr)
trConstifyDR (GenDimRange dim rng) = liftA2 GenDimRange (mapM constifyExpr dim) (constifyRange rng)

constifyDR :: DimRange -> Maybe CDimRange
constifyDR (DimRange dr) = CDimRange <$> trConstifyDR dr

trUnconstDR :: GenDimRange CExpr -> GenDimRange Expr
trUnconstDR (GenDimRange dim rng) = GenDimRange (map unconstExpr dim) (unconstRange rng)

unconstDR :: CDimRange -> DimRange
unconstDR (CDimRange dr) = DimRange (trUnconstDR dr)

-- | converts variable lvalue into net lvalue
constifyLV :: VarLValue -> Maybe NetLValue
constifyLV v = case v of
  LVSingle hi mdr -> LVSingle hi <$> maybe (Just Nothing) (fmap Just . constifyDR) mdr
  LVConcat l -> LVConcat <$> mapM constifyLV l

unconstLV :: NetLValue -> VarLValue
unconstLV n = case n of
  LVSingle hi dr -> LVSingle hi (unconstDR <$> dr)
  LVConcat l -> LVConcat (NE.map unconstLV l)

-- | converts expression into net lvalue
expr2netlv :: Expr -> Maybe NetLValue
expr2netlv (Expr x) = aux x
  where
    aux x = case x of
      ExprPrim (PrimConcat c) -> LVConcat <$> mapM aux c
      ExprPrim (PrimIdent s sub) -> LVSingle s <$> maybe (Just Nothing) (Just . constifyDR) sub
      _ -> Nothing

netlv2expr :: NetLValue -> Expr
netlv2expr = Expr . aux
  where
    aux x = case x of
      LVConcat e -> ExprPrim $ PrimConcat $ NE.map aux e
      LVSingle s dr -> ExprPrim $ PrimIdent s $ unconstDR <$> dr

-- | Common types for variables, parameters, functions and tasks
comType :: Parser ComType
comType = fproduce $ \t -> case t of
  KWInteger -> Just CTInteger
  KWReal -> Just CTReal
  KWRealtime -> Just CTRealtime
  KWTime -> Just CTTime
  _ -> Nothing

-- | Function return and parameter types
funparType :: Parser FunParType
funparType = FPTComType <$> comType <|> FPTSignRange <$> signRange

-- | Task and function argument types
taskfunType :: Parser TaskFunType
taskfunType = TFTComType <$> comType <|> liftA2 TFTRegSignRange (optConsume KWReg) signRange

-- | Net types
netType :: LProduce NetType
netType =
  [ (KWSupply0, NTSupply0),
    (KWSupply1, NTSupply1),
    (KWTri, NTTri),
    (KWTriand, NTTriAnd),
    (KWTrior, NTTriOr),
    (KWTri0, NTTri0),
    (KWTri1, NTTri1),
    (KWUwire, NTUwire),
    (KWWire, NTWire),
    (KWWand, NTWAnd),
    (KWWor, NTWOr)
  ]

-- | Parses local and nonlocal parameters declarations
trParamDecl :: Bool -> [Attribute] -> Parser (NonEmpty Parameter)
trParamDecl safety a =
  funparType
    >>= \t -> srcsl1 safety ident $ \s -> consume SymEq *> (Parameter a s t <$> mtm constExpr)

paramDecl :: Bool -> [Attribute] -> Parser [Parameter]
paramDecl b a = NE.toList <$> trParamDecl b a <* consume SymSemi

-- | Function and task input arguments
funArgDecl :: Bool -> LABranch [AttrIded TaskFunType]
funArgDecl safety =
  [ ( KWInput,
      \a ->
        taskfunType >>= \kind ->
          NE.toList
            <$> srcsl1
              safety
              ident
              (\s -> return $ AttrIded a s kind)
    )
  ]

taskArgDecl :: Bool -> LABranch [AttrIded (Dir, TaskFunType)]
taskArgDecl safety =
  [ (KWInput, pp DirIn),
    (KWInout, pp DirInOut),
    (KWOutput, pp DirOut)
  ]
  where
    pp dir a =
      taskfunType >>= \kind ->
        NE.toList
          <$> srcsl1
            safety
            ident
            (\s -> return $ AttrIded a s (dir, kind))

-- | Delay1/2/3
delayCom :: Parser NumIdent
delayCom = fproduce $ \t -> case t of
  LitDecimal i -> Just $ NINumber i
  LitReal s -> Just $ NIReal s
  IdSimple s -> Just $ NIIdent s
  IdEscaped s -> Just $ NIIdent s
  _ -> Nothing

delay1 :: Parser Delay1
delay1 = D1Base <$> delayCom <|> D11 <$> parens (mtm expr)

delay2 :: Parser Delay2
delay2 =
  consume SymPound
    *> ( D2Base <$> delayCom
           <|> parens (mtm expr >>= \a -> option (D21 a) $ consume SymComma *> (D22 a <$> mtm expr))
       )

delay3 :: Parser Delay3
delay3 =
  consume SymPound
    *> ( D3Base <$> delayCom
           <|> ( pcsl1 (mtm expr) >>= \x -> case x of
                   a :| [] -> return $ D31 a
                   a :| [b] -> return $ D32 a b
                   a :| [b, c] -> return $ D33 a b c
                   _ -> hardfail "A delay cannot have more than 3 elemets"
               )
       )

-- | Drive strength
strength :: Parser (Either Bool (Strength, Bool))
strength = fproduce $ \t -> case t of
  KWSupply0 -> Just $ Right (StrSupply, False)
  KWSupply1 -> Just $ Right (StrSupply, True)
  KWStrong0 -> Just $ Right (StrStrong, False)
  KWStrong1 -> Just $ Right (StrStrong, True)
  KWPull0 -> Just $ Right (StrPull, False)
  KWPull1 -> Just $ Right (StrPull, True)
  KWWeak0 -> Just $ Right (StrWeak, False)
  KWWeak1 -> Just $ Right (StrWeak, True)
  KWHighz0 -> Just $ Left False
  KWHighz1 -> Just $ Left True
  _ -> Nothing

comDriveStrength :: Parser DriveStrength
comDriveStrength = do
  s1 <- strength
  consume SymComma
  s2 <- strength
  case (s1, s2) of
    (Left _, Left _) -> hardfail "Only one of the strength can be high impedence"
    (Left bl, Right (s, br)) ->
      if bl == br
        then hardfail "Both strength must refer to a different value"
        else return DSHighZ {_DSHZ = bl, _DSStr = s}
    (Right (s, br), Left bl) ->
      if bl == br
        then hardfail "Both strength must refer to a different value"
        else return DSHighZ {_DSHZ = bl, _DSStr = s}
    (Right (s1, b1), Right (s2, b2)) -> case (b1, b2) of
      (False, True) -> return DSNormal {_DS0 = s1, _DS1 = s2}
      (True, False) -> return DSNormal {_DS0 = s2, _DS1 = s1}
      _ -> hardfail "Both strength must refer to a different value"

driveStrength :: Parser DriveStrength
driveStrength = option dsDefault $ try $ parens comDriveStrength

-- | Best effort PATHPULSE parser
-- It parses way more than it's supposed to, I give up
pathpulse :: Parser SpecParamDecl
pathpulse = do
  mid <- fproduce $ \t -> case t of TknPP s -> Just s; _ -> Nothing
  iid <- if B.null mid then option "" ident else return mid
  (ist, ost) <-
    if B.null iid
      then return (defaultIdM, defaultIdM)
      else do
        irng <- optionMaybe constRangeExpr
        oid <-
          option "" $
            (if isJust irng then id else optional) (consume SymDollar) *> ident
              <|> fproduce (\t -> case t of IdSystem s -> Just s; _ -> Nothing)
        orng <- optionMaybe constRangeExpr
        return (Identified iid irng, Identified oid orng)
  consume SymEq
  parens $ do
    rej <- mtm constExpr
    err <- option rej (consume SymComma *> mtm constExpr)
    return $ SPDPathPulse ist ost rej err

-- | Specify parameter declaration
specParam :: Parser (NonEmpty SpecParam)
specParam =
  optionMaybe range2 >>= \rng ->
    rcsl1 $
      SpecParam rng
        <$> (liftA2 (\s -> SPDAssign . Identified s) ident (consume SymEq *> mtm constExpr) <|> pathpulse)

-- | Event control
eventControl :: Parser EventControl
eventControl = fbranch $ \t -> case t of
  SymAster -> Just $ return ECDeps
  SymParenAster -> Just $ consume SymParenR *> return ECDeps -- yeah, f*** that
  IdSimple s -> Just $ ECIdent <$> trHierIdent False s
  IdEscaped s -> Just $ ECIdent <$> trHierIdent False s
  SymParenL ->
    Just $
      ( consume SymAster *> return ECDeps <|> ECExpr . NE.fromList
          <$> sepBy1
            ( do
                p <- option EPAny $
                  fproduce $ \t -> case t of
                    KWPosedge -> Just EPPos
                    KWNegedge -> Just EPNeg
                    _ -> Nothing
                EventPrim p <$> expr
            )
            (fproduce (\t -> case t of SymComma -> Just (); KWOr -> Just (); _ -> Nothing))
      )
        <* consume SymParenR
  _ -> Nothing

-- | Statement utility functions
contass :: Parser ProcContAssign -> Parser Statement
contass p = SProcContAssign <$> p <* consume SymSemi

stmtLoop :: Parser LoopStatement -> Parser Statement
stmtLoop p = liftA2 SLoop p attrStmt

stmtBlock :: Bool -> Parser Statement
stmtBlock kind = do
  ms <- optionMaybe $ consume SymColon *> ident
  pos <- getPosition
  (p, lp, d) <- stdBlockDecl
  h <- case ms of
    Just s -> return $ Just (s, p, lp, d)
    Nothing ->
      if null p && null lp && null d
        then return Nothing
        else do
          warn
            pos
            "Declaration in an unnamed sequential or parallel statement block is a SystemVerilog feature"
          return $
            Just
              ( B.pack $
                  map c2w $
                    printf "__block_name_for_declarations_at_line_%d__" (sourceLine pos),
                p,
                lp,
                d
              )
  SBlock h kind <$> many attrStmt <* consume (if kind then KWJoin else KWEnd)

caseX :: ZOX -> Parser Statement
caseX zox = do
  cond <- parens expr
  (s, l) <-
    pci [] >>= \l0 ->
      (if null l0 then id else option (Attributed [] Nothing, l0)) $
        consume KWDefault *> optional (consume SymColon) *> mkpair optStmt (pci l0)
  consume KWEndcase
  return $ SCase zox cond l s
  where
    pci l = accrmany (:) l $ liftA2 CaseItem (csl1 expr) $ consume SymColon *> optStmt

blockass :: VarLValue -> Parser Statement
blockass lv = do
  bl <- fproduce $ \t -> case t of SymEq -> Just True; SymLtEq -> Just False; _ -> Nothing
  delev <- optionMaybe $
    fbranch $ \t -> case t of
      SymPound -> Just (DECDelay <$> delay1)
      SymAt -> Just (DECEvent <$> eventControl)
      KWRepeat -> Just $ do
        e <- parens expr
        consume SymAt
        ev <- eventControl
        return DECRepeat {_DECRExpr = e, _DECREvent = ev}
      _ -> Nothing
  e <- expr
  consume SymSemi
  return $ SBlockAssign bl (VarAssign lv e) delev

-- | Statement
statement :: Parser Statement
statement = fbranch $ \t -> case t of
  SymPound -> Just $ liftA2 (SProcTimingControl . Left) delay1 optStmt
  SymAt -> Just $ liftA2 (SProcTimingControl . Right) eventControl optStmt
  SymDashGt ->
    Just $ liftA2 SEventTrigger (hierIdent True) (many $ brackets expr) <* consume SymSemi
  KWFork -> Just $ stmtBlock True
  KWBegin -> Just $ stmtBlock False
  KWCase -> Just $ caseX ZOXO
  KWCasez -> Just $ caseX ZOXZ
  KWCasex -> Just $ caseX ZOXX
  KWDisable -> Just $ SDisable <$> hierIdent False <* consume SymSemi
  KWWait -> Just $ liftA2 SWait (parens expr) optStmt
  KWIf -> Just $ do
    cond <- parens expr
    tb <- optStmt
    pos <- getPosition
    fb <-
      option (Attributed [] Nothing) $
        consume KWElse *> optStmt
    if stmtDanglingElse fb tb
      then warn pos "Nested ifs followed by a single else is ambiguous (dangling else)"
      else return ()
    return $ SIf cond tb fb
  KWAssign -> Just $ contass $ PCAAssign <$> varAssign
  KWDeassign -> Just $ contass $ PCADeassign <$> varLV
  KWForce -> Just $
    contass $ do
      va <- varAssign
      return $
        PCAForce $ case constifyLV (_VALValue va) of
          Just nl -> Right (NetAssign nl $ _VAValue va)
          Nothing -> Left va
  KWRelease -> Just $
    contass $ do
      vl <- varLV
      return $
        PCARelease $ case constifyLV vl of
          Just nl -> Right nl
          Nothing -> Left vl
  KWForever -> Just $ stmtLoop $ return LSForever
  KWRepeat -> Just $ stmtLoop $ LSRepeat <$> parens expr
  KWWhile -> Just $ stmtLoop $ LSWhile <$> parens expr
  KWFor ->
    Just $
      stmtLoop $
        parens $
          liftA3 LSFor varAssign (consume SymSemi *> expr) (consume SymSemi *> varAssign)
  IdSystem s ->
    Just $
      SSysTaskEnable s <$> option [] (NE.toList <$> pcsl1 (optionMaybe expr)) <* consume SymSemi
  IdSimple s -> Just $ blockassortask s
  IdEscaped s -> Just $ blockassortask s
  SymBraceL -> Just $ (LVConcat <$> csl1 varLV <* consume SymBraceR) >>= blockass
  _ -> Nothing
  where
    blockassortask s =
      trHierIdent True s >>= \hi ->
        (dimRange >>= blockass . LVSingle hi)
          <|> (STaskEnable hi <$> (option [] $ parens $ wcsl1 "task argument" expr) <* consume SymSemi)

attrStmt :: Parser AttrStmt
attrStmt = liftA2 Attributed attributes statement

optStmt :: Parser MybStmt
optStmt = liftA2 Attributed attributes $ Just <$> statement <|> consume SymSemi *> return Nothing

-- | Block declarations except parameters, including local parameters
blockDecl :: Parser t -> LABranch (Either (NonEmpty Parameter) (NonEmpty (AttrIded (BlockDecl t))))
blockDecl p =
  maplaproduce
    (<* consume SymSemi)
    [ ( KWReg,
        \a -> do
          sr <- signRange
          ppl p (BDReg sr) a
      ),
      (KWInteger, ppl p BDInt),
      (KWReal, ppl p BDReal),
      (KWTime, ppl p BDTime),
      (KWRealtime, ppl p BDRealTime),
      (KWEvent, ppl (many range2) BDEvent),
      (KWLocalparam, \a -> Left <$> trParamDecl False a)
    ]
  where
    ppl ps f a = Right <$> rcsl1 (liftA2 (AttrIded a) ident $ f <$> ps)

-- | Standard block declarations
stdBlockDecl :: Parser ([Parameter], [Parameter], StdBlockDecl)
stdBlockDecl =
  monoAccum $
    try
      ( mkpair attributes $
          lproduce $
            mapabranch (\x -> (x, [], [])) (KWParameter, paramDecl False) :
            maplabranch
              ( \bdl -> case bdl of
                  Left lp -> ([], NE.toList lp, [])
                  Right d -> ([], [], NE.toList d)
              )
              (blockDecl $ many range2)
      )
      >>= \(a, p) -> p a

-- | Gate instantiation utility functions
gateInst :: (NetLValue -> (NonEmpty Expr) -> Maybe GateInst) -> Parser (NonEmpty ModGenItem)
gateInst f =
  rcsl1 $
    sz2Ref >>= \sr ->
      parens
        ( do
            out <- netLV
            consume SymComma
            args <- csl1 expr
            case f out args of
              Just inst -> return $ uncurry MGIGateInst sr inst
              Nothing -> hardfail "Unexpected arguments"
        )

-- | Gates fitting the gateInst facility
gateCmos :: Bool -> Parser (NonEmpty ModGenItem)
gateCmos r =
  optionMaybe delay3 >>= \d3 ->
    gateInst
      ( \out args -> case args of
          inp :| [ncon, pcon] -> Just $ GICMos r d3 out inp ncon pcon
          _ -> Nothing
      )

gateEnable :: Bool -> Bool -> Parser (NonEmpty ModGenItem)
gateEnable r b = do
  ds <- driveStrength
  d3 <- optionMaybe delay3
  gateInst $ \out args -> case args of
    inp :| [en] -> Just $ GIEnable r b ds d3 out inp en
    _ -> Nothing

gateMos :: Bool -> Bool -> Parser (NonEmpty ModGenItem)
gateMos r np =
  optionMaybe delay3 >>= \d3 ->
    gateInst
      ( \out args -> case args of
          inp :| [en] -> Just $ GIMos r np d3 out inp en
          _ -> Nothing
      )

gateNinp :: NInputType -> Bool -> Parser (NonEmpty ModGenItem)
gateNinp t n = do
  ds <- driveStrength
  d2 <- optionMaybe delay2
  gateInst $ \out inp -> Just $ GINIn t n ds d2 out inp

-- | Gates not fitting the gateInst facility
gateNout :: Bool -> Parser (NonEmpty ModGenItem)
gateNout r = do
  ds <- driveStrength
  d2 <- optionMaybe delay2
  rcsl1 $
    sz2Ref >>= \sr ->
      parens
        ( do
            out <- netLV
            consume SymComma
            inp :| args <- rcsl1 expr
            case rmapM expr2netlv args of
              Just outs -> return $ uncurry MGIGateInst sr $ GINOut r ds d2 (out :| outs) inp
              Nothing -> hardfail "Expecting a net lvalue for all but the last argument"
        )

gatePassen :: Bool -> Bool -> Parser (NonEmpty ModGenItem)
gatePassen r b =
  optionMaybe delay2 >>= \d2 ->
    rcsl1
      ( sz2Ref >>= \sr ->
          parens $
            liftA3
              (\lh rh -> uncurry MGIGateInst sr . GIPassEn r b d2 lh rh)
              netLV
              (consume SymComma *> netLV)
              (consume SymComma *> expr)
      )

gatePass :: Bool -> Parser (NonEmpty ModGenItem)
gatePass r =
  rcsl1 $
    sz2Ref >>= \sr ->
      parens $
        liftA2 (\lh -> uncurry MGIGateInst sr . GIPass r lh) netLV (consume SymComma *> netLV)

gatePull :: Bool -> Parser (NonEmpty ModGenItem)
gatePull ud =
  ( option dsDefault $
      try $
        parens $ do
          e1 <- strength
          e2 <- optionMaybe (consume SymComma *> strength)
          case (e1, e2) of
            (Left b, Nothing) | ud == b -> return DSHighZ {_DSHZ = b, _DSStr = StrStrong}
            (Left bl, Just (Right (s, br))) | bl /= br -> return DSHighZ {_DSHZ = bl, _DSStr = s}
            (Right (s, br), Just (Left bl)) | bl /= br -> return DSHighZ {_DSHZ = bl, _DSStr = s}
            (Right (s, b), Nothing)
              | ud == b ->
                return $
                  if b
                    then DSNormal {_DS0 = StrStrong, _DS1 = s}
                    else DSNormal {_DS1 = StrStrong, _DS0 = s}
            (Right (s1, False), Just (Right (s2, True))) -> return DSNormal {_DS0 = s1, _DS1 = s2}
            (Right (s1, True), Just (Right (s2, False))) -> return DSNormal {_DS0 = s2, _DS1 = s1}
            _ -> hardfail "Unexpected arguments"
  )
    >>= \ds -> rcsl1 (sz2Ref >>= \sr -> parens (uncurry MGIGateInst sr . GIPull ud ds <$> netLV))

-- | Task and function declaration common parts
taskfunDecl ::
  Bool ->
  (Bool -> LABranch [AttrIded a]) ->
  Parser (B.ByteString, [AttrIded a], ([Parameter], [Parameter], StdBlockDecl))
taskfunDecl zeroarg arglb = do
  s <- ident
  p <-
    do
      port <-
        parens $
          (if zeroarg then rcsl else wrcsl1 "function parameters") $
            labranch $ arglb True
      return $ (,,) s (concat port) <$> stdBlockDecl
      <|> return
        ( (uncurry $ (,,) s)
            <$> ( monoAccum $
                    labranch $
                      mapabranch (\x -> ([], (x, [], []))) (KWParameter, paramDecl False) :
                      maplaproduce (\p -> (\e -> (e, ([], [], []))) <$> p <* consume SymSemi) (arglb False)
                        ++ maplabranch
                          ( \bdl -> case bdl of
                              Left lp -> ([], ([], NE.toList lp, []))
                              Right d -> ([], ([], [], NE.toList d))
                          )
                          (blockDecl $ many range2)
                )
        )
  consume SymSemi
  p

-- | Net and trireg declaration
comNetTrireg ::
  Maybe (Expr -> NetKind) -> Maybe ([Range2] -> NetKind) -> [Attribute] -> Parser GenerateItem
comNetTrireg fl fr a = do
  vs <- optionMaybe $
    fproduce $ \t -> case t of
      KWVectored -> Just True
      KWScalared -> Just False
      _ -> Nothing
  sn <- optConsume KWSigned
  vec <-
    if isJust vs
      then Just . (,) vs <$> range2
      else optionMaybe ((,) vs <$> range2)
  d3 <- optionMaybe delay3
  let mkelem s t = AttrIded a s $ MGDNet t sn vec d3
  hdid <- ident
  ehdt <- consume SymEq *> (Left <$> expr) <|> Right <$> many range2
  l <- case (ehdt, fl, fr) of
    (Left hdt, Just f, _) ->
      accrmany (<|) (mkelem hdid (f hdt) :| []) $
        consume SymComma *> liftA2 mkelem ident (consume SymEq *> (f <$> expr))
    (Right hdt, _, Just f) ->
      accrmany (<|) (mkelem hdid (f hdt) :| []) $
        consume SymComma *> liftA2 mkelem ident (f <$> many range2)
    _ -> hardfail "Got mixed elements of trireg and standard net"
  consume SymSemi
  return $ GIMGD l

-- | Net declaration
netDecl :: NetType -> [Attribute] -> Parser GenerateItem
netDecl nt a =
  optionMaybe (parens comDriveStrength) >>= \ods ->
    uncurry
      comNetTrireg
      ( case ods of
          Just ds -> (Just $ \e -> NKNet nt $ Right (ds, e), Nothing)
          Nothing -> (Just $ \e -> NKNet nt $ Right (dsDefault, e), Just $ \d -> NKNet nt $ Left d)
      )
      a

-- | Trireg declaration
triregDecl :: [Attribute] -> Parser GenerateItem
triregDecl a = do
  ods_cs <-
    optionMaybe $
      parens $
        Left <$> comDriveStrength <|> Right
          <$> fproduce
            ( \t -> case t of
                KWSmall -> Just CSSmall
                KWMedium -> Just CSMedium
                KWLarge -> Just CSLarge
                _ -> Nothing
            )
  uncurry
    comNetTrireg
    ( case ods_cs of
        Nothing -> (Just $ \e -> NKTriD dsDefault e, Just $ \d -> NKTriC CSMedium d)
        Just (Left ds) -> (Just $ \e -> NKTriD ds e, Nothing)
        Just (Right cs) -> (Nothing, Just $ \d -> NKTriC cs d)
    )
    a

mkI :: Parser (NonEmpty ModGenItem) -> [Attribute] -> Parser GenerateItem
mkI p a = GIMGI . NE.map (Attributed a) <$> p <* consume SymSemi

-- | Module or Generate region statement
comModGenItem :: LABranch GenerateItem
comModGenItem =
  maplabranch
    ( \bdl -> case bdl of
        Left lp -> GIParam lp
        Right r -> GIMGD $ NE.map (\ai -> ai {_AIData = MGDBlockDecl $ _AIData ai}) r
    )
    (blockDecl (consume SymEq *> (Right <$> constExpr) <|> Left <$> many range2))
    ++ maplproduce netDecl netType
    ++ [ (KWCmos, mkI $ gateCmos False),
         (KWRcmos, mkI $ gateCmos True),
         (KWBufif0, mkI $ gateEnable False False),
         (KWBufif1, mkI $ gateEnable False True),
         (KWNotif0, mkI $ gateEnable True False),
         (KWNotif1, mkI $ gateEnable True True),
         (KWNmos, mkI $ gateMos False True),
         (KWPmos, mkI $ gateMos False False),
         (KWRnmos, mkI $ gateMos True True),
         (KWRpmos, mkI $ gateMos True False),
         (KWAnd, mkI $ gateNinp NITAnd False),
         (KWNand, mkI $ gateNinp NITAnd True),
         (KWOr, mkI $ gateNinp NITOr False),
         (KWNor, mkI $ gateNinp NITOr True),
         (KWXor, mkI $ gateNinp NITXor False),
         (KWXnor, mkI $ gateNinp NITXor True),
         (KWBuf, mkI $ gateNout False),
         (KWNot, mkI $ gateNout True),
         (KWTranif0, mkI $ gatePassen False False),
         (KWTranif1, mkI $ gatePassen False True),
         (KWRtranif0, mkI $ gatePassen True False),
         (KWRtranif1, mkI $ gatePassen True True),
         (KWTran, mkI $ gatePass False),
         (KWRtran, mkI $ gatePass True),
         (KWPulldown, mkI $ gatePull False),
         (KWPullup, mkI $ gatePull True),
         (KWInitial, mkIs $ MGIInitial <$> attrStmt),
         (KWAlways, mkIs $ MGIAlways <$> attrStmt),
         (KWTrireg, triregDecl),
         (KWGenvar, \a -> GIMGD <$> rcsl1 ((flip (AttrIded a) MGDGenVar) <$> ident) <* consume SymSemi),
         ( KWIf,
           \a -> do
             cond <- parens constExpr
             tb <- optGenBlock
             pos <- getPosition
             fb <- option Nothing $ consume KWElse *> (Just <$> optGenBlock)
             if genDanglingElse (maybe (Just $ GBBlock $ Identified "" mempty) id fb) tb
               then warn pos "Nested ifs followed by a single else is ambiguous (dangling else)"
               else return ()
             return $ GIMGI $ Attributed a (MGIIf cond tb $ join fb) :| []
         ),
         ( KWBegin,
           \a -> do
             pos <- getPosition
             warn pos "Generate blocks outside of for/if/case is a Verilog 2001 feature disallowed by later standards"
             s <- option "" (consume SymColon *> ident)
             gr <- genReg
             consume KWEnd
             return $
               GIMGI $
                 Attributed
                   []
                   ( MGIIf
                       (CExpr $ ExprPrim $ PrimNumber Nothing False $ NDecimal 1)
                       (Just $ GBBlock $ Identified s gr)
                       Nothing
                   )
                   :| []
         ),
         ( KWAssign,
           mkI $ do
             str <- driveStrength
             del <- optionMaybe delay3
             rcsl1 $ liftA2 (\lv -> MGIContAss str del . NetAssign lv) netLV $ consume SymEq *> expr
         ),
         ( KWDefparam,
           \a ->
             GIParamOver
               <$> rcsl1
                 (liftA2 (ParamOver a) (hierIdent False) (consume SymEq *> mtm constExpr))
               <* consume SymSemi
         ),
         ( KWCase,
           \a -> do
             cond <- parens constExpr
             (d, b) <-
               accrmany (:) [] cbranch >>= \cb ->
                 (if null cb then id else option (Nothing, cb)) $
                   consume KWDefault *> option () (consume SymColon)
                     *> mkpair optGenBlock (accrmany (:) cb cbranch)
             consume KWEndcase
             return $ GIMGI $ Attributed a (MGICase cond b d) :| []
         ),
         ( KWFor,
           \a ->
             GIMGI . (:| []) . Attributed a
               <$> ( parens
                       ( liftA3
                           MGILoopGen
                           (liftA2 Identified ident $ consume SymEq *> constExpr)
                           (consume SymSemi *> constExpr)
                           (liftA2 Identified (consume SymSemi *> ident) (consume SymEq *> constExpr))
                       )
                       <*> genBlock
                   )
         ),
         ( KWTask,
           \a -> do
             auto <- optConsume KWAutomatic
             (ss, port, (param, localparam, decl)) <- taskfunDecl True taskArgDecl
             s <- optStmt
             consume KWEndtask
             return $ GIMGD $ AttrIded a ss (MGDTask auto port param localparam decl s) :| []
         ),
         ( KWFunction,
           \a -> do
             auto <- optConsume KWAutomatic
             kind <- optionMaybe funparType
             (ss, port, (param, localparam, decl)) <- taskfunDecl False funArgDecl
             s <- statement -- well, technically incorrect but whatever
             consume KWEndfunction
             return $ GIMGD $ AttrIded a ss (MGDFunc auto kind port param localparam decl s) :| []
         )
       ]
  where
    mkIs p a = GIMGI . (:| []) . Attributed a <$> p
    cbranch = liftA2 GenCaseItem (csl1 constExpr) $ consume SymColon *> optGenBlock

-- | Port instantiation relying on order
ordPort :: [Attribute] -> Parser (Attributed (Maybe Expr))
ordPort a = Attributed a <$> optionMaybe expr

-- | Port instantiation relying on name
namePort :: [Attribute] -> Parser (AttrIded (Maybe Expr))
namePort a = consume SymDot *> liftA2 (AttrIded a) ident (parens $ optionMaybe expr)

-- | Utilitary function to convert an unknown instantiation to module, udp or left it unknown
mkudpargs :: PortAssign -> Maybe (NetLValue, NonEmpty Expr)
mkudpargs a = case a of
  PortPositional (t@(_ : _ : _)) -> do
    h : args <-
      rmapM
        ( \ame -> case ame of
            Attributed [] x -> x
            _ -> Nothing
        )
        t
    lv <- expr2netlv h
    return (lv, NE.fromList $ reverse args)
  _ -> Nothing

-- | Module or udp instantiation is tricky to differentiate
modudpInst :: Parser (NonEmpty ModGenItem)
modudpInst = do
  kind <- lenientIdent
  ds <- optionMaybe $ try $ parens comDriveStrength
  del_par <-
    optionMaybe $
      consume SymPound
        *> ( Left . Left . D2Base <$> delayCom
               <|> parens
                 ( Left . Right . ParamNamed
                     <$> xcsl1 (consume SymDot *> liftA2 Identified ident (parens $ optionMaybe $ mtm expr))
                     <|> ( mtm expr >>= xrcsl "parameter instantiation or delay specification" (mtm expr) . (:| [])
                             >>= \l -> case nermapM (\p -> case p of MTMSingle e -> Just e; _ -> Nothing) l of
                               Nothing -> case l of
                                 a :| [] -> return $ Left $ Left $ D21 a
                                 a :| [b] -> return $ Left $ Left $ D22 b a
                                 _ -> hardfail "Delay expression cannot have more than 2 elements"
                               Just (a :| []) -> return $ Right $ Left a
                               Just (a :| [b]) -> return $ Right $ Right (a, b)
                               Just l -> return $ Left $ Right $ ParamPositional $ NE.toList l
                         )
                 )
           )
  insts <- rcsl1 $ do
    (s, rng) <- sz2Ref
    args <- parens $
      option (PortPositional []) $ do
        a <- attributes
        h <- Left . (:| []) <$> namePort a <|> Right . (:| []) <$> ordPort a
        case h of
          Left l -> PortNamed . NE.toList <$> xrcsl "port connections" (attributes >>= namePort) l
          Right l ->
            PortPositional . NE.toList
              <$> option l (accrmany (<|) l $ consume SymComma >> attributes >>= ordPort)
    return (s, rng, args)
  let isnotmod = isJust ds || any (\(s, _, _) -> B.null s) insts
      mkmod =
        if isnotmod
          then Nothing
          else case del_par of
            Just (Left (Right pa)) -> Just $ MGIModInst kind pa
            Just (Right (Left pa)) -> Just $ MGIModInst kind $ ParamPositional [pa]
            Just (Right (Right (pa0, pa1))) ->
              Just $ MGIModInst kind $ ParamPositional [pa0, pa1]
            Nothing -> Just $ MGIModInst kind $ ParamNamed []
            _ -> Nothing
      udpds = maybe dsDefault id ds
      mkudp = case (isnotmod, del_par) of
        (_, Just (Left (Left d2))) -> Just $ MGIUDPInst kind udpds $ Just d2
        (False, Just (Right dp)) -> Just $ MGIUnknownInst kind $ Just dp
        (True, Just (Right (Left d1))) -> Just $ MGIUDPInst kind udpds $ Just $ D21 $ MTMSingle d1
        (True, Just (Right (Right (d0, d1)))) ->
          Just $ MGIUDPInst kind udpds $ Just $ D22 (MTMSingle d0) (MTMSingle d1)
        (False, Nothing) -> Just $ MGIUnknownInst kind Nothing
        (True, Nothing) -> Just $ MGIUDPInst kind udpds Nothing
        _ -> Nothing
      (hs, hr, ha) :| t = insts
  case foldl'
    (\acc (s, r, a) -> acc >>= \(f, l) -> (\(lv, a) -> (f, f s r lv a <| l)) <$> mkudpargs a)
    (mkudp >>= \f -> mkudpargs ha >>= \(lv, a) -> return (f, f hs hr lv a :| []))
    t of
    Just (_, it) -> return $ NE.reverse it
    Nothing -> case mkmod of
      Just f -> return $ NE.map (\(s, r, a) -> f s r a) insts
      Nothing -> hardfail "Got mixed elements of module and udp instatiations"
  where
    xcsl1 p = do
      x <- p
      pos <- getPosition
      b <- optConsume SymComma
      if b
        then
          (x :) <$> xcsl1 p <|> do
            warn pos "Extraneous comma at the end of parameter instantiation is not correct Verilog"
            return [x]
        else return [x]
    xrcsl s p l = do
      pos <- getPosition
      b <- optConsume SymComma
      if b
        then
          (p >>= xrcsl s p . (<| l)) <|> do
            warn pos $ printf "Extraneous comma at the end of %s is not correct Verilog" (s :: String)
            return l
        else return l

-- | Parse a module or generate region item along other given possibilities
-- and converts it to the right type using the provided conversion function
parseItem :: (GenerateItem -> a) -> LABranch a -> Parser a
parseItem f lb =
  attributes
    >>= \a -> (lproduce (lb ++ maplabranch f comModGenItem) >>= \p -> p a) <|> f <$> mkI modudpInst a

-- | Generate region
genReg :: Parser GenerateRegion
genReg =
  monoAccum $
    parseItem
      ( \x -> case x of
          GIParam p -> mempty {_GRLocalParam = NE.toList p}
          GIParamOver o -> mempty {_GRDefParam = NE.toList o}
          GIMGD d -> mempty {_GRDecl = NE.toList d}
          GIMGI i -> mempty {_GRBody = NE.toList i}
      )
      []

-- | Generate block
genBlock :: Parser GenerateBlock
genBlock =
  consume KWBegin
    *> liftA2
      (\s -> GBBlock . Identified s)
      (option "" $ consume SymColon *> ident)
      (genReg <* consume KWEnd)
    <|> parseItem GBSingle []

optGenBlock :: Parser (Maybe GenerateBlock)
optGenBlock = Just <$> genBlock <|> consume SymSemi *> return Nothing

-- | Port declaration
portDecl :: Bool -> LABranch [AttrIded PortDecl]
portDecl safety =
  [ (KWInput, pnsl PDIn),
    (KWInout, pnsl PDInOut),
    ( KWOutput,
      \a ->
        pnsl PDOut a
          <|> fbranch
            ( \t -> case t of
                KWReg -> Just $ signRange >>= pv a . PDOutReg
                KWInteger -> Just $ pv a $ PDOutVar True
                KWTime -> Just $ pv a $ PDOutVar False
                _ -> Nothing
            )
    )
  ]
  where
    pnsl f a = do
      v <- liftA2 f (optionMaybe $ lproduce netType) signRange
      NE.toList <$> srcsl1 safety ident (pure . flip (AttrIded a) v)
    pv a f =
      NE.toList
        <$> srcsl1
          safety
          ident
          (\s -> AttrIded a s . f <$> optionMaybe (consume SymEq *> constExpr))

-- | Port expression
portExpr :: Parser [Identified (Maybe CRangeExpr)]
portExpr = option [] $ (: []) <$> pp <|> NE.toList <$> bcsl1 pp
  where
    pp = liftA2 Identified ident $ optionMaybe constRangeExpr

-- | Path declaration
trPathDecl :: ModulePathCondition -> Parser [SpecifyItem]
trPathDecl cond = do
  edge <- optionMaybe $
    fproduce $ \t -> case t of
      KWPosedge -> Just True
      KWNegedge -> Just False
      _ -> Nothing
  inp <- csl1 specTerm
  po <- optionMaybe $
    fproduce $ \t -> case t of
      SymPlus -> Just True
      SymDash -> Just False
      _ -> Nothing
  pf <- fproduce $ \t -> case t of
    SymAsterGt -> Just False
    SymEqGt -> Just True
    _ -> Nothing
  let cons = SIPathDeclaration cond
  pd <-
    ( parens
        ( if isJust po
            then hardfail "Misplaced polarity operator"
            else do
              outp <- csl1 specTerm
              po <- fbranch $ \t -> case t of
                SymPlus -> Just $ consume SymColon *> return (Just True)
                SymDash -> Just $ consume SymColon *> return (Just False)
                SymColon -> Just $ return Nothing
                SymPlusColon -> Just $ return $ Just True
                SymDashColon -> Just $ return $ Just False
                _ -> Nothing
              e <- expr
              if pf
                then case (inp, outp) of
                  (i :| [], o :| []) -> return $ cons (SPParallel i o) po (Just (e, edge))
                  _ -> hardfail "Parallel delay only accept single nets as source and destination"
                else return $ cons (SPFull inp outp) po $ Just (e, edge)
        )
        <|> if isJust edge
          then hardfail "Mixed edge and non edge sensitive delay path elements"
          else
            csl1 specTerm >>= \outp ->
              if pf
                then case (inp, outp) of
                  (i :| [], o :| []) -> return $ cons (SPParallel i o) po Nothing
                  _ -> hardfail "Parallel delay only accept single nets as source and destination"
                else return $ cons (SPFull inp outp) po Nothing
      )
  consume SymParenR
  consume SymEq
  vals <- try (csl1 $ mtm constExpr) <|> pcsl1 (mtm constExpr) -- cancer optional parentheses
  if elem (length vals) [1, 2, 3, 6, 12]
    then return [pd vals]
    else hardfail "Wrong number of argument"

pathDecl :: ModulePathCondition -> Parser [SpecifyItem]
pathDecl mpc = consume SymParenL *> trPathDecl mpc

-- | Timing check event
edgeDesc :: Parser EdgeDesc
edgeDesc = fbranch $ \t -> case t of
  KWPosedge ->
    Just $
      return $
        V.fromList
          [True, True, False, False, False, False, False, True, False, False]
  KWNegedge ->
    Just $
      return $
        V.fromList
          [False, False, False, True, True, False, True, False, False, False]
  KWEdge ->
    Just $
      (V.replicate 10 False V.//) . NE.toList
        <$> brackets
          ( csl1 $
              fproduce $ \t -> case t of
                EdgeEdge BXZ0 BXZ1 -> Just (0, True)
                EdgeEdge BXZ0 BXZX -> Just (1, True)
                EdgeEdge BXZ0 BXZZ -> Just (2, True)
                EdgeEdge BXZ1 BXZ0 -> Just (3, True)
                EdgeEdge BXZ1 BXZX -> Just (4, True)
                EdgeEdge BXZ1 BXZZ -> Just (5, True)
                EdgeEdge BXZX BXZ0 -> Just (6, True)
                EdgeEdge BXZX BXZ1 -> Just (7, True)
                EdgeEdge BXZZ BXZ0 -> Just (8, True)
                EdgeEdge BXZZ BXZ1 -> Just (9, True)
                _ -> Nothing
          )
  _ -> Nothing

-- | Timing check condition after &&&, this thing is cancer because of ambiguous parens and tilde
timingCheckCond :: Parser (Bool, Expr)
timingCheckCond = consume SymAmpAmpAmp *> (try p <|> parens p)
  where
    p = try (consume UnTilde *> ((,) True <$> expr)) <|> (,) False <$> expr

timingCheckEvent :: Parser TimingCheckEvent
timingCheckEvent =
  liftA3 TimingCheckEvent (optionMaybe edgeDesc) specTerm (optionMaybe timingCheckCond)

controlledTimingCheckEvent :: Parser ControlledTimingCheckEvent
controlledTimingCheckEvent =
  liftA3 ControlledTimingCheckEvent edgeDesc specTerm (optionMaybe timingCheckCond)

-- | Standard system timing check arguments
optoptChain :: a -> b -> Parser a -> Parser b -> Parser (a, b)
optoptChain dx dn px pn = option (dx, dn) $ consume SymComma *> mkpair (option dx px) pn

comStcArgs :: Parser (TimingCheckEvent, TimingCheckEvent, Expr)
comStcArgs =
  liftA3 (,,) timingCheckEvent (consume SymComma *> timingCheckEvent) (consume SymComma *> expr)

stdStcArgs :: Parser STCArgs
stdStcArgs =
  comStcArgs >>= \(r, d, e) -> STCArgs d r e <$> option "" (consume SymComma *> option "" ident)

addStcArgs :: Parser (STCArgs, STCAddArgs)
addStcArgs = do
  (r, d, ll) <- comStcArgs
  consume SymComma
  lr <- expr
  let def1 = (defaultIdM, defaultIdM)
      def2 = (Nothing, def1)
      def3 = (Nothing, def2)
  (n, (sc, (cc, (dr, dd)))) <-
    optoptChain "" def3 ident $
      optoptChain Nothing def2 (optionMaybe (mtm expr)) $
        optoptChain Nothing def1 (optionMaybe (mtm expr)) $
          optoptChain defaultIdM defaultIdM cmtmRef $
            option defaultIdM (consume SymComma *> option defaultIdM cmtmRef)
  return (STCArgs d r ll n, STCAddArgs lr sc cc dr dd)

skewStcArgs :: Parser (B.ByteString, Maybe CExpr, Maybe CExpr)
skewStcArgs = do
  (n, (eb, ra)) <-
    optoptChain "" (Nothing, Nothing) ident $
      optoptChain Nothing Nothing (optionMaybe constExpr) $
        option Nothing (consume SymComma *> optionMaybe constExpr)
  return (n, eb, ra)

-- | System timing check functions
stcfMap :: HashMap.HashMap B.ByteString (Parser SystemTimingCheck)
stcfMap =
  HashMap.fromList
    [ ("setup", STCSetup . (\(STCArgs d r e n) -> STCArgs r d e n) <$> stdStcArgs),
      ("hold", STCHold <$> stdStcArgs),
      ("setuphold", uncurry STCSetupHold <$> addStcArgs),
      ("recovery", STCRecovery <$> stdStcArgs),
      ("removal", STCRemoval <$> stdStcArgs),
      ("recrem", uncurry STCRecrem <$> addStcArgs),
      ("skew", STCSkew <$> stdStcArgs),
      ( "timeskew",
        do
          (r, d, e) <- comStcArgs
          (n, eb, ra) <- skewStcArgs
          return $ STCTimeSkew (STCArgs d r e n) eb ra
      ),
      ( "fullskew",
        do
          (r, d, e) <- comStcArgs
          consume SymComma
          tcl <- expr
          (n, eb, ra) <- skewStcArgs
          return $ STCFullSkew (STCArgs d r e n) tcl eb ra
      ),
      ( "period",
        liftA3 STCPeriod controlledTimingCheckEvent (consume SymComma *> expr) $
          option "" $ consume SymComma *> option "" ident
      ),
      ( "width",
        do
          e <- controlledTimingCheckEvent
          consume SymComma
          tcl <- expr
          (t, n) <-
            option (Nothing, "") $
              consume SymComma
                *> mkpair (Just <$> constExpr) (option "" $ consume SymComma *> option "" ident)
          return $ STCWidth e tcl t n
      ),
      ( "nochange",
        liftA5
          STCNoChange
          timingCheckEvent
          (consume SymComma *> timingCheckEvent)
          (consume SymComma *> mtm expr)
          (consume SymComma *> mtm expr)
          $ option "" $ consume SymComma *> option "" ident
      )
    ]

-- | Specify block item
specifyBlock :: Parser ([SpecParam], [SpecifyItem])
specifyBlock =
  fbranch
    ( \t -> case t of
        KWSpecparam -> Just $ (\sp -> (NE.toList sp, [])) <$> specParam
        KWPulsestyleonevent -> Just $ psi $ SIPulsestyle True
        KWPulsestyleondetect -> Just $ psi $ SIPulsestyle False
        KWShowcancelled -> Just $ psi $ SIShowcancelled True
        KWNoshowcancelled -> Just $ psi $ SIShowcancelled False
        KWIf -> Just $ do
          c <- parens $
            genExpr return (pure ()) $
              \(Expr e) -> trConstifyGenExpr constifyIdent (maybe (Just ()) $ const Nothing) e
          i <- pathDecl $ MPCCond c
          return ([], i)
        KWIfnone -> Just $ (,) [] <$> pathDecl MPCNone
        SymParenL -> Just $ (,) [] <$> trPathDecl MPCAlways
        IdSystem s -> fmap ((,) [] . (: []) . SISystemTimingCheck) . parens <$> HashMap.lookup s stcfMap
        _ -> Nothing
    )
    <* consume SymSemi
  where
    psi f = (\i -> ([], NE.toList i)) <$> csl1 (f <$> specTerm)

-- | Non port declaration module item
npmodItem :: LABranch ([ParamOver], ModuleBlock)
npmodItem =
  mapabranch (\p -> ([], mempty {_MBParam = p})) (KWParameter, paramDecl False) :
  [ ( KWSpecparam,
      \a ->
        (\p -> ([], mempty {_MBSpecParam = rmap (Attributed a) $ NE.toList p}))
          <$> specParam <* consume SymSemi
    ),
    ( KWGenerate,
      \a ->
        if not (null a)
          then hardfail "Generate region doesn't accept attributes"
          else (\it -> ([], mempty {_MBBody = [MIGenReg it]})) <$> genReg <* consume KWEndgenerate
    ),
    ( KWSpecify,
      \a ->
        if not (null a)
          then hardfail "Specify region doesn't accept attributes"
          else
            (\(sp, b) -> ([], mempty {_MBBody = [MISpecBlock sp b]}))
              <$> monoAccum specifyBlock <* consume KWEndspecify
    )
  ]

mgi2mb :: GenerateItem -> ([ParamOver], ModuleBlock)
mgi2mb x = case x of
  GIParam p -> ([], mempty {_MBLocalParam = NE.toList p})
  GIParamOver o -> (NE.toList o, mempty)
  GIMGD d -> ([], mempty {_MBDecl = NE.toList d})
  GIMGI i -> ([], mempty {_MBBody = map MIMGI $ NE.toList i})

-- | Port declaration or module item
modItem :: LABranch ([ParamOver], ModuleBlock)
modItem =
  maplaproduce
    (\p -> (\x -> ([], mempty {_MBPortDecl = x})) <$> p <* consume SymSemi)
    (portDecl False)
    ++ npmodItem

-- | Parses a module
parseModule :: LocalCompDir -> [Attribute] -> Parser Verilog2005
parseModule lcd a = do
  s <- lenientIdent
  params <-
    option [] $
      consume SymPound
        *> parens
          ( xrcsl1 "parameter" [] $
              consume KWParameter *> (NE.toList <$> trParamDecl True [])
          )
  -- if there is nothing the standard says it's np
  -- if there is () the standard says it can be np or standard so standard because we can't know
  p <-
    option (parseModItem [] [] npmodItem) $
      parens $
        flip (uncurry parseModItem) npmodItem
          . unzip
          . map (\x@(AttrIded _ i _) -> (Identified i [Identified i Nothing], x))
          <$> xrcsl1 "port" [] (labranch $ portDecl True)
          <|> do
            pi <-
              rcsl $
                (consume SymDot *> liftA2 Identified ident (parens portExpr))
                  <|> (\l -> case l of [Identified s _] -> Identified s l; _ -> Identified "" l)
                    <$> portExpr
            return $ parseModItem pi [] modItem
  consume SymSemi
  (po, m) <- p
  let modu =
        m
          { _MBAttr = a,
            _MBIdent = s,
            _MBParam = _MBParam m ++ params,
            _MBTimescale = _LCDTimescale lcd,
            _MBCell = _LCDCell lcd,
            _MBPull = _LCDPull lcd,
            _MBDefNetType = _LCDDefNetType lcd
          }
      paramtopnames = getModuleParamTopNames modu
      poscope =
        map
          ( \p ->
              let pid = _POIdent p
               in if HashSet.member
                    (case _HIPath pid of hid : _ -> _IdentIdent hid; [] -> _HIIdent pid)
                    paramtopnames
                    then p {_POIdent = pid {_HIPath = Identified s Nothing : _HIPath pid}}
                    else p
          )
          po
  consume KWEndmodule
  return mempty {_VModule = [modu], _VDefParam = poscope}
  where
    parseModItem i d l = do
      (po, m) <- monoAccum (parseItem mgi2mb l)
      return (po, m {_MBPortInter = i, _MBPortDecl = _MBPortDecl m ++ d})
    xrcsl1 s acc p = do
      l <- (acc <>) <$> p
      pos <- getPosition
      b <- optConsume SymComma
      if b
        then
          xrcsl1 s l p <|> do
            warn pos $
              printf
                "Extraneous comma at the end of named %s declaration is not correct Verilog"
                (s :: String)
            return l
        else return l

-- | Primitive output port
udpOutput :: [Attribute] -> Parser (AttrIded (Maybe (Either CExpr [Attribute])))
udpOutput a = do
  reg <- optConsume KWReg
  s <- ident
  AttrIded a s
    <$> if reg
      then Just <$> option (Right a) (consume SymEq *> (Left <$> constExpr))
      else return Nothing

-- | Parse a list of udp port declarations in any order
udpPorts ::
  B.ByteString ->
  NonEmpty B.ByteString ->
  Parser (AttrIded (Maybe (Either CExpr [Attribute])), NonEmpty ([Attribute], B.ByteString))
udpPorts out ins = do
  (mo, mr, mia) <- loop Nothing Nothing HashMap.empty
  -- preserve order, otherwise dumping the map would be easier
  let lia = NE.map (\x -> (HashMap.findWithDefault [] x mia, x)) ins
  case mo of
    Nothing -> hardfail "An output port must be declared"
    Just (AttrIded a s o) ->
      return
        ( AttrIded
            a
            s
            ( case (mr, o) of
                (Just _, Just e) -> Just $ Left e
                (Just a', Nothing) -> Just $ Right a'
                (Nothing, Nothing) -> Nothing
            ),
          lia
        )
  where
    inset = HashSet.fromList $ NE.toList ins
    loop mo mr mia = do
      a <- attributes
      (mo', mr', mia') <- fbranch $ \t -> case t of
        KWReg -> Just $ do
          s <- ident
          if s /= out || isJust mr
            then hardfail "Only the first port can be declared as reg once"
            else return (mo, Just a, mia)
        KWInput ->
          Just $
            let loop' hs = do
                  s <- ident
                  if HashMap.member s hs || not (HashSet.member s inset)
                    then hardfail "Only input ports can be declared as input once"
                    else let hs' = HashMap.insert s a hs in option hs' $ consume SymComma *> loop' hs'
             in (,,) mo mr <$> loop' mia
        KWOutput -> Just $ do
          AttrIded _ s x <- udpOutput a
          if s /= out || isJust mo
            then hardfail "Only the first port can be declared as output once"
            else case x of
              Nothing -> return (Just $ AttrIded a s Nothing, mr, mia)
              Just x ->
                if isJust mr
                  then hardfail "A port cannot be declared as reg several times"
                  else
                    return
                      ( Just $
                          AttrIded a s $
                            case x of Left e -> Just e; Right _ -> Nothing,
                        Just a,
                        mia
                      )
        _ -> Nothing
      consume SymSemi
      option (mo', mr', mia') $ loop mo' mr' mia'

-- | Sequential primitive output initial value
initVal :: Parser ZOX
initVal = fbranch $ \t -> case t of
  LitDecimal 0 -> Just $ return ZOXZ
  LitDecimal 1 -> Just $
    option ZOXO $ do
      consume (NumberBase False BBin)
      fproduce $ \t -> case t of
        LitBinary [BXZ0] -> Just ZOXZ
        LitBinary [BXZ1] -> Just ZOXO
        LitBinary [BXZX] -> Just ZOXX
        _ -> Nothing
  _ -> Nothing

-- | Signal level
level :: Parser SigLevel
level = fproduce $ \t -> case t of
  TableOut ZOXZ -> Just L0
  TableOut ZOXO -> Just L1
  TableOut ZOXX -> Just LX
  TableIn False -> Just LQ
  TableIn True -> Just LB
  _ -> Nothing

-- | Sequential primitive nest state
nextState :: Parser (Maybe ZOX)
nextState =
  fproduce
    ( \t -> case t of
        TableOut zox -> Just $ Just zox
        SymDash -> Just Nothing
        _ -> Nothing
    )
    <* consume SymSemi

-- | Sequential primitive input row
seqRow :: Parser SeqIn
seqRow =
  ( many level >>= \comb ->
      (if null comb then id else option (SIComb $ NE.fromList comb))
        ( do
            e <- fbranch $ \t -> case t of
              TableEdge AFRNPA -> Just $ return $ EdgeDesc LQ LQ
              TableEdge AFRNPF -> Just $ return $ EdgeDesc L1 L0
              TableEdge AFRNPR -> Just $ return $ EdgeDesc L0 L1
              TableEdge AFRNPN -> Just $ return (EdgePos_neg False)
              TableEdge AFRNPP -> Just $ return (EdgePos_neg True)
              SymParenL -> Just $ liftA2 EdgeDesc level level <* consume SymParenR
              _ -> Nothing
            SISeq comb e <$> many level
        )
  )
    <* consume SymColon

-- | Parses a primitive block
udp :: [Attribute] -> Parser Verilog2005
udp attr = do
  udpid <- ident
  p <-
    parens $
      liftA2 udpPorts ident (consume SymComma *> csl1 ident)
        <|> do
          attr <- attributes
          consume KWOutput
          out <- udpOutput attr
          consume SymComma
          ins <- csl1 $ do
            attr <- attributes
            consume KWInput
            l <- srcsl1 True ident $ return . (,) attr
            return $ NE.toList l
          return $ return (out, NE.fromList $ concat ins)
  consume SymSemi
  (AttrIded outa outs sout, ins) <- p
  init <- optionMaybe $ do
    consume KWInitial
    s <- ident
    if s /= outs
      then hardfail "Initial value can only be assigned to output port"
      else consume SymEq *> initVal <* consume SymSemi
  consume KWTable
  hdi <- seqRow
  sl <- level
  hdd <- fbranch $ \t -> case t of
    SymSemi -> Just $ return Nothing
    SymColon -> Just $ Just <$> nextState
    _ -> Nothing
  let hdu = case sl of L0 -> Just ZOXZ; L1 -> Just ZOXO; LX -> Just ZOXX; _ -> Nothing
  body <- case (sout, hdd, hdi, hdu) of
    (Nothing, Nothing, SIComb comb, Just zox)
      | init == Nothing ->
        CombTable . (CombRow comb zox :|)
          <$> many
            ( liftA2
                (CombRow . NE.fromList)
                (many1 level)
                ( consume SymColon
                    *> fproduce (\t -> case t of TableOut zox -> Just zox; _ -> Nothing)
                )
                <* consume SymSemi
            )
    (Just ox, Just ns, _, _) ->
      SeqTable ox init . (SeqRow hdi sl ns :|)
        <$> many (liftA3 SeqRow seqRow level $ consume SymColon *> nextState)
    _ -> hardfail "Got mixed information between sequential and combinatorial UDP"
  consume KWEndtable
  consume KWEndprimitive
  return mempty {_VPrimitive = [PrimitiveBlock attr udpid (outa, outs) ins body]}

-- | Parses an element of config blocks body
configItem :: Parser ConfigItem
configItem = do
  ci <- fbranch $ \t -> case t of
    KWCell -> Just $ Left <$> dot1Ident
    KWInstance -> Just $ Right . NE.fromList <$> sepBy1 ident (consume SymDot)
    _ -> Nothing
  llu <- fbranch $ \t -> case t of
    KWLiblist -> Just $ LLULiblist <$> many ident
    KWUse ->
      Just $
        liftA2
          LLUUse
          dot1Ident
          (option False $ consume SymColon *> consume KWConfig *> return True)
    _ -> Nothing
  consume SymSemi
  return $ ConfigItem ci llu

-- | Parses a config block
config :: Parser Verilog2005
config = do
  s <- ident
  consume SymSemi
  consume KWDesign
  design <- many dot1Ident
  consume SymSemi
  b <- accrmany (:) [] $ configItem
  (body, d) <- option (b, []) $ do
    consume KWDefault
    consume KWLiblist
    d <- many ident
    consume SymSemi
    body <- accrmany (:) b $ configItem
    return (body, d)
  consume KWEndconfig
  return mempty {_VConfig = [ConfigBlock s design body d]}

-- | Parses compiler directives
compDir :: Parser ()
compDir =
  branchPrim
    ( \t -> case t of
        CDUnconnecteddrive ->
          Just $
            branchPrim
              ( \t -> case t of
                  KWPull0 -> Just $ modifyState $ \s -> s {_LCDPull = Just False}
                  KWPull1 -> Just $ modifyState $ \s -> s {_LCDPull = Just True}
                  _ -> Nothing
              )
        CDNounconnecteddrive -> Just $ modifyState $ \s -> s {_LCDPull = Nothing}
        CDDefaultnettype -> Just $ do
          nt <-
            Just <$> producePrim (\t -> IntMap.lookup (getConsIndex t) $ mkActionMap netType)
              <|> producePrim (\t -> if t == IdSimple "none" then Just Nothing else Nothing)
          modifyState $ \s -> s {_LCDDefNetType = nt}
        CDResetall -> Just $ putState lcdDefault
        CDTimescale -> Just $ do
          uoff <- producePrim $ \t -> case t of CDTSInt i -> Just i; _ -> Nothing
          ubase <- producePrim $ \t -> case t of CDTSUnit i -> Just i; _ -> Nothing
          poff <- producePrim $ \t -> case t of CDTSInt i -> Just i; _ -> Nothing
          pbase <- producePrim $ \t -> case t of CDTSUnit i -> Just i; _ -> Nothing
          modifyState $ \s -> s {_LCDTimescale = Just (ubase + uoff, pbase + poff)}
        _ -> Nothing
    )
    *> anywherecompdir

-- | Parses a top-level declaration: config, module or primitive block
topDecl :: Parser Verilog2005
topDecl =
  skipMany1 compDir *> return mempty <|> do
    a <- concat <$> (many $ attribute <* skipMany compDir)
    st <- getState
    fbranch $ \t -> case (a, t) of
      (_, KWPrimitive) -> Just $ udp a
      (_, KWModule) -> Just $ parseModule st a
      (_, KWMacromodule) -> Just $ parseModule st a
      ([], KWConfig) -> Just config
      _ -> Nothing

-- | Parses a verilog file by accumulating top-level declarations
verilog2005Parser :: Parser Verilog2005
verilog2005Parser = anywherecompdir *> monoAccum topDecl <* eof

-- | Parse a file containing Verilog 2005 code, also return non Verilog2005 conformity warnings
-- The lists in the Verilog2005 structure are in reverse order from source
parseVerilog2005 :: FilePath -> IO (Verilog2005, [String])
parseVerilog2005 file = do
  src <- LB.readFile file
  case alexScanTokens src of
    Left s -> error $ file ++ "\n" ++ s
    Right strm ->
      let (x, w) = runWriter $ runParserT verilog2005Parser lcdDefault file strm
       in case x of
            Left s -> error $ show s
            Right ast -> return (ast, if null w then w else ("In " ++ file ++ ":") : w)
