-- Module      : Verismith.Verilog2005.PrettyPrinter
-- Description : Pretty printer for the Verilog 2005 AST.
-- Copyright   : (c) 2023 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : experimental
-- Portability : POSIX
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}

module Verismith.Verilog2005.PrettyPrinter
  ( genSource,
    LocalCompDir (..),
    lcdDefault,
    lcdTimescale,
    lcdCell,
    lcdPull,
    lcdDefNetType,
  )
where

import Control.Lens.TH
import Data.Bifunctor (first)
import Data.Functor.Compose
import Data.Functor.Identity
import qualified Data.ByteString as B
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy as LB
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, fromMaybe)
import Data.String
import qualified Data.Vector.Unboxed as V
import Verismith.Utils hiding (comma)
import Verismith.Verilog2005.AST
import Verismith.Verilog2005.LibPretty
import Verismith.Verilog2005.Utils

-- | All locally applicable properties controlled by compiler directives
data LocalCompDir = LocalCompDir
  { _lcdTimescale :: Maybe (Int, Int),
    _lcdCell :: Bool,
    _lcdPull :: Maybe Bool,
    _lcdDefNetType :: Maybe NetType
  }

$(makeLenses ''LocalCompDir)

lcdDefault :: LocalCompDir
lcdDefault = LocalCompDir Nothing False Nothing $ Just NTWire

-- | Generates a string from a line length limit and a Verilog AST
genSource :: Maybe Word -> Verilog2005 -> LB.ByteString
genSource mw = layout mw . prettyVerilog2005

-- | Comma separated concatenation
(<.>) :: Doc -> Doc -> Doc
(<.>) a b = a <> comma <+> b

-- | [] {} ()
brk :: Doc -> Doc
brk = encl lbracket rbracket

brc :: Doc -> Doc
brc = encl lbrace rbrace

par :: Doc -> Doc
par = encl lparen rparen

gpar :: Doc -> Doc
gpar = group . par

-- | Print only if boolean is false
piff :: Doc -> Bool -> Doc
piff d c = if c then mempty else d

-- | Print only if boolean is true
pift :: Doc -> Bool -> Doc
pift d c = if c then d else mempty

-- | Regroup and prettyprint
prettyregroup ::
  (Doc -> Doc -> Doc) ->
  (y -> Doc) ->
  (x -> y) ->
  (x -> y -> Maybe y) ->
  NonEmpty x ->
  Doc
prettyregroup g p mk add l = pl g p $ regroup mk add l

-- | The core difficulty of this pretty printer:
-- | Identifier can be escaped, escaped identifiers require a space or newline after them
-- | but I want to avoid double [spaces/line breaks] and spaces at the end of a line
-- | And this problem creeps in every AST node with possible identifier printed at the end
-- | like expressions
-- | So a prettyprinter that takes an AST node of type `a` has this type
-- | The second element of the result pair is the type of space that follows the first element
-- | unless what is next is a space or a newline, in which case it can be ignored and replaced
type PrettyIdent a = a -> (Doc, Doc)

-- | The `pure/return` of prettyIdent
mkid :: PrettyIdent Doc
mkid x = (x, softline)

-- | Evaluates a PrettyIdent and collapse the last character
padj :: PrettyIdent a -> a -> Doc
padj p = uncurry (<>) . p

-- | Adds stuff before the last character
fpadj :: (Doc -> Doc) -> PrettyIdent a -> a -> Doc
fpadj f p = uncurry (<>) . first f . p

gpadj :: PrettyIdent a -> a -> Doc
gpadj = fpadj group

ngpadj :: PrettyIdent a -> a -> Doc
ngpadj = fpadj ng

-- | Just inserts a group before the last space of a PrettyIdent
-- | Useful to separate the break priority of the inside and outside space
mkg :: PrettyIdent x -> PrettyIdent x
mkg f = first group . f

-- | In this version the group asks for raising the indentation level
mkng :: PrettyIdent x -> PrettyIdent x
mkng f = first ng . f

-- | Print a comma separated list, haskell style: `a, b, c` or
-- | ```
-- | a
-- | , b
-- | , c
-- | ```
csl :: Foldable f => Doc -> Doc -> (a -> Doc) -> f a -> Doc
csl = pf $ \a b -> a </> comma <+> b

csl1 :: (a -> Doc) -> NonEmpty a -> Doc
csl1 f = foldrMap1 f $ \a b -> f a </> comma <+> b

cslid1 :: PrettyIdent a -> PrettyIdent (NonEmpty a)
cslid1 f = foldrMap1 f $ \a -> first (padj f a <.>)

cslid :: Foldable f => Doc -> Doc -> PrettyIdent a -> f a -> Doc
cslid a b f = nonEmpty mempty (\l -> a <> padj (cslid1 f) l <> b) . toList

bcslid1 :: PrettyIdent a -> NonEmpty a -> Doc
bcslid1 f = brc . nest . padj (cslid1 $ mkg f)

pcslid :: Foldable f => PrettyIdent a -> f a -> Doc
pcslid f = cslid (lparen <> softspace) rparen $ mkg f

prettyBS :: PrettyIdent B.ByteString
prettyBS i = (raw i, if B.head i == c2w '\\' then newline else softline)

prettyIdent :: PrettyIdent Identifier
prettyIdent (Identifier i) = prettyBS i

rawId :: Identifier -> Doc
rawId (Identifier i) = raw i

-- | Somehow the six next function are very common patterns
pspWith :: PrettyIdent a -> Doc -> PrettyIdent a
pspWith f d i = if nullDoc d then f i else mkid $ fst (f i) <> newline <> d

padjWith :: PrettyIdent a -> Doc -> PrettyIdent a
padjWith f d i = if nullDoc d then f i else mkid $ padj f i <> d

prettyEq :: Doc -> (Doc, Doc) -> (Doc, Doc)
prettyEq i = first $ \x -> ng $ group i <=> equals <+> group x

prettyAttrng :: [Attribute] -> Doc -> Doc
prettyAttrng a x = prettyAttr a <?=> ng x

prettyItem :: [Attribute] -> Doc -> Doc
prettyItem a x = prettyAttrng a x <> semi

prettyItems :: Doc -> (a -> Doc) -> NonEmpty a -> Doc
prettyItems h f b = group h <=> group (csl1 (ng . f) b) <> semi

prettyItemsid :: Doc -> PrettyIdent a -> NonEmpty a -> Doc
prettyItemsid h f b = group h <=> gpadj (cslid1 $ mkng f) b <> semi

prettyAttr :: [Attribute] -> Doc
prettyAttr = nonEmpty mempty $ \l -> group $ "(* " <> fst (cslid1 pa l) <=> "*)"
  where
    pa (Attribute i e) = maybe (prettyBS i) (prettyEq (raw i) . pca) e
    pca = prettyGExpr prettyIdent (pm prettyCRangeExpr) (const mempty) 12

prettyHierIdent :: PrettyIdent HierIdent
prettyHierIdent (HierIdent p i) = first (\i -> nest $ foldr addId i p) $ prettyIdent i
  where
    addId (s, r) acc =
      ngpadj (padjWith prettyIdent (pm (group . brk . padj prettyCExpr) r)) s <> dot <> acc

prettyDot1Ident :: PrettyIdent Dot1Ident
prettyDot1Ident (Dot1Ident h t) =
  first (\ft -> case h of Nothing -> ft; Just h -> padj prettyBS h <> dot <> ft) $ prettyIdent t

prettySpecTerm :: PrettyIdent SpecTerm
prettySpecTerm (SpecTerm i r) = padjWith prettyIdent (pm prettyCRangeExpr r) i

prettyNumber :: Number -> Doc
prettyNumber x = case x of
  NBinary l -> "b" </> fromString (concatMap show l)
  NOctal l -> "o" </> fromString (concatMap show l)
  NDecimal i -> "d" </> viaShow i
  NHex l -> "h" </> fromString (concatMap show l)
  NXZ b -> if b then "dx" else "dz"

prettyNumIdent :: PrettyIdent NumIdent
prettyNumIdent x = case x of
  NIIdent i -> prettyIdent i
  NIReal r -> mkid $ raw r
  NINumber n -> mkid $ viaShow n

prettyPrim :: PrettyIdent i -> (r -> Doc) -> (a -> Doc) -> PrettyIdent (GenPrim i r a)
prettyPrim ppid ppr ppa x = case x of
  PrimNumber Nothing True (NDecimal i) -> mkid $ viaShow i
  PrimNumber w b n ->
    mkid $
      nest $
        pm (\w -> viaShow w <> softline) w
          <> group ((if b then "'s" else squote) <> prettyNumber n)
  PrimReal r -> mkid $ raw r
  PrimIdent i r -> first nest $ padjWith ppid (group $ ppr r) i
  PrimConcat l -> mkid $ bcslid1 pexpr l
  PrimMultConcat e l ->
    mkid $ brc $ nest $
      gpadj (prettyGExpr prettyIdent (pm prettyCRangeExpr) ppa 12) e <> bcslid1 pexpr l
  PrimFun i a l ->
    let da = ppa a in (if nullDoc da then padjWith else pspWith) ppid (da <?=> pcslid pexpr l) i
  PrimSysFun i l -> first (\x -> nest $ "$" <> x) $ padjWith prettyBS (pcslid pexpr l) i
  PrimMinTypMax m -> mkid $ par $ padj (prettyGMTM pexpr) m
  PrimString x -> mkid $ raw $ let dq = c2w '\"' in B.cons dq $ B.snoc x dq
  where
    pexpr = prettyGExpr ppid ppr ppa 12

preclevel :: BinaryOperator -> Int
preclevel b = case b of
  BinPower -> 1
  BinTimes -> 2
  BinDiv -> 2
  BinMod -> 2
  BinPlus -> 3
  BinMinus -> 3
  BinLSL -> 4
  BinLSR -> 4
  BinASL -> 4
  BinASR -> 4
  BinLT -> 5
  BinLEq -> 5
  BinGT -> 5
  BinGEq -> 5
  BinEq -> 6
  BinNEq -> 6
  BinCEq -> 6
  BinCNEq -> 6
  BinAnd -> 7
  BinXor -> 8
  BinXNor -> 8
  BinOr -> 9
  BinLAnd -> 10
  BinLOr -> 11

prettyGExpr :: PrettyIdent i -> (r -> Doc) -> (a -> Doc) -> Int -> PrettyIdent (GenExpr i r a)
prettyGExpr ppid ppr ppa l e = case e of
  ExprPrim e -> first group $ prettyPrim ppid ppr ppa e
  ExprUnOp op a e ->
    let da = ppa a
        (x, s) = prettyPrim ppid ppr ppa e
     in (ng $ viaShow op <> piff (space <> da <> newline) (nullDoc da) <> x, s)
  ExprBinOp el op a r ->
    let p = preclevel op
        x = psexpr p el <=> viaShow op
        da = ppa a
        pp = pexpr $ p - 1
     in case compare l p of
          LT -> mkid $ ng $ par $ x <+> (da <?=> padj pp r)
          EQ -> first (\y -> x <+> (da <?=> y)) $ pp r
          GT -> first (\y -> ng $ x <+> (da <?=> y)) $ pp r
  ExprCond ec a et ef ->
    let pc c t f = nest $ group (c <=> nest ("?" <?+> ppa a)) <=> group (t <=> colon <+> f)
        pp = first (pc (psexpr 11 ec) (psexpr 12 et)) $ pexpr 12 ef
     in if l < 12 then mkid $ gpar $ uncurry (<>) $ pp else pp
  where
    pexpr = prettyGExpr ppid ppr ppa
    psexpr n = fst . pexpr n

prettyExpr :: PrettyIdent Expr
prettyExpr (Expr e) = prettyGExpr prettyHierIdent (pm prettyDimRange) prettyAttr 12 e

prettyCExpr :: PrettyIdent CExpr
prettyCExpr (CExpr e) = prettyGExpr prettyIdent (pm prettyCRangeExpr) prettyAttr 12 e

prettyGMTM :: PrettyIdent et -> PrettyIdent (GenMinTypMax et)
prettyGMTM pp x = case x of
  MTMSingle e -> pp e
  MTMFull l t h -> first (\x -> gpadj pp l <> colon <-> gpadj pp t <> colon <-> group x) $ pp h

prettyMTM :: PrettyIdent MinTypMax
prettyMTM = prettyGMTM prettyExpr

prettyCMTM :: PrettyIdent CMinTypMax
prettyCMTM = prettyGMTM prettyCExpr

prettyRange2 :: Range2 -> Doc
prettyRange2 (Range2 m l) = brk $ gpadj prettyCExpr m <> colon <-> gpadj prettyCExpr l

prettyR2s :: [Range2] -> Doc
prettyR2s = pl (</>) $ group . prettyRange2

prettyRangeExpr :: PrettyIdent e -> GenRangeExpr e -> Doc
prettyRangeExpr pp x = case x of
  GRESingle r -> brk $ padj pp r
  GREPair r2 -> prettyRange2 r2
  GREBaseOff b mp o ->
    brk $ gpadj pp b <> (if mp then "-" else "+") <> colon <-> gpadj prettyCExpr o

prettyCRangeExpr :: CRangeExpr -> Doc
prettyCRangeExpr = prettyRangeExpr prettyCExpr

prettyGDR :: PrettyIdent e -> GenDimRange e -> Doc
prettyGDR pp (GenDimRange d r) =
  foldr ((</>) . group . brk . padj pp) (group $ prettyRangeExpr pp r) d

prettyDimRange :: DimRange -> Doc
prettyDimRange = prettyGDR prettyExpr

prettyCDimRange :: CDimRange -> Doc
prettyCDimRange = prettyGDR prettyCExpr

prettySignRange :: SignRange -> Doc
prettySignRange (SignRange s r) = pift "signed" s <?=> pm (group . prettyRange2) r

prettyComType :: (d -> Doc) -> ComType d -> Doc
prettyComType f x = case x of
  CTAbstract t -> viaShow t
  CTConcrete e sr -> group $ f e <?=> prettySignRange sr

prettyDriveStrength :: DriveStrength -> Doc
prettyDriveStrength x = case x of
  DSNormal StrStrong StrStrong -> mempty
  DSNormal s0 s1 -> par $ viaShow s0 <> "0" </> comma <+> viaShow s1 <> "1"
  DSHighZ False s -> par $ "highz0" </> comma <+> viaShow s <> "1"
  DSHighZ True s -> par $ viaShow s <> "0" </> comma <+> "highz1"

prettyDelay3 :: PrettyIdent Delay3
prettyDelay3 x =
  first ("#" <>) $ case x of
    D3Base ni -> prettyNumIdent ni
    D31 m -> mkid $ par $ padj prettyMTM m
    D32 m1 m2 -> mkid $ par $ ngpadj prettyMTM m1 <.> ngpadj prettyMTM m2
    D33 m1 m2 m3 -> mkid $ par $ ngpadj prettyMTM m1 <.> ngpadj prettyMTM m2 <.> ngpadj prettyMTM m3

prettyDelay2 :: PrettyIdent Delay2
prettyDelay2 x =
  first ("#" <>) $ case x of
    D2Base ni -> prettyNumIdent ni
    D21 m -> mkid $ par $ padj prettyMTM m
    D22 m1 m2 -> mkid $ par $ ngpadj prettyMTM m1 <.> ngpadj prettyMTM m2

prettyDelay1 :: PrettyIdent Delay1
prettyDelay1 x =
  first ("#" <>) $ case x of
    D1Base ni -> prettyNumIdent ni
    D11 m -> mkid $ par $ padj prettyMTM m

prettyLValue :: (dr -> Doc) -> PrettyIdent (LValue dr)
prettyLValue f x = case x of
  LVSingle hi r -> first nest $ padjWith prettyHierIdent (pm (group . f) r) hi
  LVConcat l -> mkid $ bcslid1 (prettyLValue f) l

prettyNetLV :: PrettyIdent NetLValue
prettyNetLV = prettyLValue prettyCDimRange

prettyVarLV :: PrettyIdent VarLValue
prettyVarLV = prettyLValue prettyDimRange

prettyAssign :: (dr -> Doc) -> PrettyIdent (Assign dr)
prettyAssign f (Assign l e) = prettyEq (fst $ prettyLValue f l) $ prettyExpr e

prettyNetAssign :: PrettyIdent NetAssign
prettyNetAssign = prettyAssign prettyCDimRange

prettyVarAssign :: PrettyIdent VarAssign
prettyVarAssign = prettyAssign prettyDimRange

prettyEventControl :: PrettyIdent EventControl
prettyEventControl x =
  first ("@" <>) $ case x of
    ECDeps -> ("*", newline)
    ECIdent hi -> first ng $ prettyHierIdent hi
    ECExpr l -> (gpar $ padj (cslid1 pEP) l, newline)
  where
    pEP (EventPrim p e) =
      first
        (\x -> ng $ case p of { EPAny -> mempty; EPPos -> "posedge"; EPNeg -> "negedge" } <?=> x)
        $ prettyExpr e

prettyEdgeDesc :: EdgeDesc -> Doc
prettyEdgeDesc x =
  if x == V.fromList [True, True, False, False, False, True]
    then "posedge"
    else
      if x == V.fromList [False, False, True, True, True, False]
        then "negedge"
        else group $ "edge" <=> brk (csl mempty mempty raw $ V.ifoldr pED [] x)
  where
    pED i b =
      if b
        then (:) $ case i of
          0 -> "01"
          1 -> "0x" -- or 0z
          2 -> "10"
          3 -> "1x" -- or 1z
          4 -> "x0" -- or z0
          5 -> "x1" -- or z1
        else id

prettyXparam :: Doc -> ComType () -> NonEmpty (Identified CMinTypMax) -> Doc
prettyXparam pre t =
  prettyItemsid (pre <?=> prettyComType (const mempty) t) $
    \(Identified i v) -> prettyEq (rawId i) $ prettyCMTM v

type EDI = Either [Range2] CExpr

data AllBlockDecl
  = ABDReg SignRange (NonEmpty (Identified EDI))
  | ABDInt (NonEmpty (Identified EDI))
  | ABDReal (NonEmpty (Identified EDI))
  | ABDTime (NonEmpty (Identified EDI))
  | ABDRealTime (NonEmpty (Identified EDI))
  | ABDEvent (NonEmpty (Identified [Range2]))
  | ABDLocalParam (ComType ()) (NonEmpty (Identified CMinTypMax))
  | ABDParameter (ComType ()) (NonEmpty (Identified CMinTypMax))
  | ABDPort Dir (ComType Bool) (NonEmpty Identifier)

fromBlockDecl ::
  (forall x. f x -> NonEmpty (Identified x)) -> (t -> EDI) -> BlockDecl f t -> AllBlockDecl
fromBlockDecl ff ft bd = case bd of
  BDReg sr x -> convt (ABDReg sr) x
  BDInt x -> convt ABDInt x
  BDReal x -> convt ABDReal x
  BDTime x -> convt ABDTime x
  BDRealTime x -> convt ABDRealTime x
  BDEvent r2 -> conv ABDEvent r2
  BDLocalParam t v -> conv (ABDLocalParam t) v
  where
    conv c = c . ff
    convt c = c . NE.map (\(Identified i x) -> Identified i $ ft x) . ff

fromStdBlockDecl :: AttrIded StdBlockDecl -> Attributed AllBlockDecl
fromStdBlockDecl (AttrIded a i sbd) =
  Attributed a $ case sbd of
    SBDParameter (Parameter t v) -> ABDParameter t [Identified i v]
    SBDBlockDecl bd -> fromBlockDecl ((:|[]) . Identified i . runIdentity) Left bd
    
prettyAllBlockDecl :: AllBlockDecl -> Doc
prettyAllBlockDecl x = case x of
  ABDReg sr l -> mkedi ("reg" <?=> prettySignRange sr) l
  ABDInt l -> mkedi "integer" l
  ABDReal l -> mkedi "real" l
  ABDTime l -> mkedi "time" l
  ABDRealTime l -> mkedi "realtime" l
  ABDEvent l ->
    prettyItemsid "event" (\(Identified i r) -> padjWith prettyIdent (prettyR2s r) i) l
  ABDLocalParam t l -> prettyXparam "localparam" t l
  ABDParameter t l -> prettyXparam "parameter" t l
  ABDPort d t l -> prettyItemsid (viaShow d <?=> prettyComType (pift "reg") t) prettyIdent l
  where
    mkedi h l =
      prettyItemsid
        h
        ( \(Identified i edi) -> case edi of
            Left r2 -> padjWith prettyIdent (group $ prettyR2s r2) i
            Right ce -> prettyEq (rawId i) $ prettyCExpr ce
        )
        l

prettyAllBlockDecls :: [Attributed AllBlockDecl] -> Doc
prettyAllBlockDecls =
  nonEmpty mempty $
    prettyregroup
      (<#>)
      (\(Attributed a abd) -> prettyAttrng a $ prettyAllBlockDecl abd)
      id
      (addAttributed $ \x y -> case (x, y) of
        (ABDReg nsr nl, ABDReg sr l) | nsr == sr -> Just $ ABDReg sr $ nl <> l
        (ABDInt nl, ABDInt l) -> Just $ ABDInt $ nl <> l
        (ABDReal nl, ABDReal l) -> Just $ ABDReal $ nl <> l
        (ABDTime nl, ABDTime l) -> Just $ ABDTime $ nl <> l
        (ABDRealTime nl, ABDRealTime l) -> Just $ ABDRealTime $ nl <> l
        (ABDEvent nl, ABDEvent l) -> Just $ ABDEvent $ nl <> l
        (ABDLocalParam nt nl, ABDLocalParam t l) | nt == t -> Just $ ABDLocalParam t $ nl <> l
        (ABDParameter nt nl, ABDParameter t l) | nt == t -> Just $ ABDParameter t $ nl <> l
        (ABDPort nd nt nl, ABDPort d t l) | nd == d && nt == t -> Just $ ABDPort d t $ nl <> l
        _ -> Nothing
      )

prettyStdBlockDecls :: [AttrIded StdBlockDecl] -> Doc
prettyStdBlockDecls = prettyAllBlockDecls . map fromStdBlockDecl

prettyTFBlockDecls :: (d -> Dir) -> [AttrIded (TFBlockDecl d)] -> Doc
prettyTFBlockDecls f =
  prettyAllBlockDecls
    . map
      ( \(AttrIded a i x) -> case x of
          TFBDPort d t -> Attributed a $ ABDPort (f d) t [i]
          TFBDStd sbd -> fromStdBlockDecl (AttrIded a i sbd)
      )

prettyStatement :: Bool -> Statement -> Doc
prettyStatement protect x = case x of
  SBlockAssign b (Assign lv v) dec ->
    let delevctl x = case x of
          DECRepeat e ev ->
            group ("repeat" <=> gpar (padj prettyExpr e)) <=> fst (prettyEventControl ev)
          DECDelay d -> fst $ prettyDelay1 d
          DECEvent e -> fst $ prettyEventControl e
     in ng $
          group (fst $ prettyVarLV lv)
            <=> group (piff langle b <> equals <+> pm delevctl dec <?=> gpadj prettyExpr v)
            <> semi
  SCase zox e b s ->
    block
      ( nest $
          "case" <> case zox of {ZOXZ -> "z"; ZOXO -> mempty; ZOXX -> "x"}
            <=> gpar (padj prettyExpr e)
      )
      "endcase"
      $ pl
        (<#>)
        (\(CaseItem p v) -> gpadj (cslid1 prettyExpr) p <> colon <+> ng (prettyMybStmt False v))
        b
        <?#> piff (nest $ "default:" <=> prettyMybStmt False s) (s == Attributed [] Nothing)
  SIf c t f ->
    let head = "if" <=> gpar (padj prettyExpr c)
     in case f of
          Attributed [] Nothing | protect == False -> ng head <> prettyRMybStmt False t
          -- `else` and `begin`/`fork` at same indentation level
          Attributed [] (Just x@(SBlock _ _ _)) ->
            ng (group head <> prettyRMybStmt True t) <#> "else" <=> prettyStatement False x
          Attributed [] (Just x@(SIf _ _ _)) -> -- `if` and `else if` at same indentation level
            ng (group head <> prettyRMybStmt True t) <#> "else" <=> prettyStatement protect x
          _ -> ng (group head <> prettyRMybStmt True t) <#> nest ("else" <> prettyRMybStmt protect f)
  SDisable hi -> group $ "disable" <=> padj prettyHierIdent hi <> semi
  SEventTrigger hi e ->
    group $
      "->" <+> padj prettyHierIdent hi
        <> pl (</>) (group . brk . padj prettyExpr) e
        <> semi
  SLoop ls s ->
    let pva = gpadj prettyVarAssign
        head = case ls of
          LSForever -> "forever"
          LSRepeat e -> "repeat" <=> gpar (padj prettyExpr e)
          LSWhile e -> "while" <=> gpar (padj prettyExpr e)
          LSFor i c u -> "for" <=> gpar (pva i <> semi <+> gpadj prettyExpr c <> semi <+> pva u)
     in nest $ ng head <=> prettyAttrStmt protect s
  SProcContAssign pca ->
    (<> semi) $ group $ case pca of
      PCAAssign va -> "assign" <=> padj prettyVarAssign va
      PCADeassign lv -> "deassign" <=> padj prettyVarLV lv
      PCAForce lv -> "force" <=> either (padj prettyVarAssign) (padj prettyNetAssign) lv
      PCARelease lv -> "release" <=> either (padj prettyVarLV) (padj prettyNetLV) lv
  SProcTimingControl dec s ->
    group (fst $ either prettyDelay1 prettyEventControl dec) <=> prettyMybStmt protect s
  SBlock h ps s ->
    block
      (nest $ (if ps then "fork" else "begin") <?/> pm (\(s, _) -> colon <+> rawId s) h)
      (if ps then "join" else "end")
      $ maybe mempty (prettyStdBlockDecls . snd) h <?#> pl (<#>) (prettyAttrStmt False) s
  SSysTaskEnable s a ->
    group ("$" <> padj prettyBS s <> pcslid (maybe (mkid mempty) prettyExpr) a) <> semi
  STaskEnable hi a -> group (padj prettyHierIdent hi <> pcslid prettyExpr a) <> semi
  SWait e s -> ng ("wait" <=> gpar (padj prettyExpr e)) <> prettyRMybStmt protect s

prettyAttrStmt :: Bool -> AttrStmt -> Doc
prettyAttrStmt protect (Attributed a s) = prettyAttr a <?=> nest (prettyStatement protect s)

prettyMybStmt :: Bool -> MybStmt -> Doc
prettyMybStmt protect (Attributed a s) = case s of
  Nothing -> prettyAttr a <> semi
  Just s -> prettyAttr a <?=> prettyStatement protect s

prettyRMybStmt :: Bool -> MybStmt -> Doc
prettyRMybStmt protect (Attributed a s) =
  case a of {[] -> mempty; _ -> newline <> prettyAttr a}
    <> maybe semi (\x -> newline <> prettyStatement protect x) s

prettyPortAssign :: PortAssign -> Doc
prettyPortAssign x = case x of
  PortPositional l ->
    cslid
      mempty
      mempty
      ( \(Attributed a e) -> case e of
          Nothing -> mkid $ prettyAttr a
          Just e -> first (\x -> group $ prettyAttrng a x) $ prettyExpr e
      )
      l
  PortNamed l ->
    csl
      mempty
      softline
      ( \(AttrIded a i e) ->
          group $ prettyAttrng a $ dot <> padj prettyIdent i <> gpar (pm (padj prettyExpr) e)
      )
      l

prettyModGenSingleItem :: ModGenSingleItem -> Bool -> Doc
prettyModGenSingleItem x protect = case x of
  MGINetInit nt ds np l -> prettyItemsid (viaShow nt <?=> prettyDriveStrength ds <?=> com np) pide l
  MGINetDecl nt np l -> prettyItemsid (viaShow nt <?=> com np) pidd l
  MGITriD ds np l -> prettyItemsid ("trireg" <?=> prettyDriveStrength ds <?=> com np) pide l
  MGITriC cs np l ->
    prettyItemsid ("trireg" <?=> piff (viaShow cs) (cs == CSMedium) <?=> com np) pidd l
  MGIBlockDecl bd -> prettyAllBlockDecl $ fromBlockDecl getCompose id bd
  MGIGenVar l -> prettyItemsid "genvar" prettyIdent l
  MGITask aut s d b ->
    block
      (ng $ group ("task" <?=> mauto aut) <=> padj prettyIdent s <> semi)
      "endtask"
      (prettyTFBlockDecls id d <?#> nest (prettyMybStmt False b))
  MGIFunc aut t s d b ->
    block
      ( ng $
          group ("function" <?=> mauto aut <?=> pm (prettyComType $ const mempty) t)
            <=> padj prettyIdent s
            <> semi
      )
      "endfunction"
      (prettyTFBlockDecls (const DirIn) d <?#> nest (prettyStatement False $ toStatement b))
  MGIDefParam l ->
    prettyItemsid
      "defparam"
      (\(ParamOver hi v) -> prettyEq (fst $ prettyHierIdent hi) $ prettyCMTM v)
      l
  MGIContAss ds d3 l ->
    prettyItemsid ("assign" <?=> prettyDriveStrength ds <?=> pm (fst . prettyDelay3) d3) prettyNetAssign l
  MGICMos r d3 l ->
    prettyItems
      (pift "r" r <> "cmos" <?=> pm (fst . prettyDelay3) d3)
      ( \(GICMos n lv inp nc pc) ->
          pm pname n
            <> gpar
              ( ngpadj prettyNetLV lv
                  <.> ngpadj prettyExpr inp
                  <.> ngpadj prettyExpr nc
                  <.> ngpadj prettyExpr pc
              )
      )
      l
  MGIEnable r oz ds d3 l ->
    prettyItems
      ( (if r then "not" else "buf") <> "if" <> (if oz then "1" else "0")
          <?=> prettyDriveStrength ds
          <?=> pm (fst . prettyDelay3) d3
      )
      ( \(GIEnable n lv inp e) ->
          pm pname n <> gpar (ngpadj prettyNetLV lv <.> ngpadj prettyExpr inp <.> ngpadj prettyExpr e)
      )
      l
  MGIMos r np d3 l ->
    prettyItems
      (pift "r" r <> (if np then "n" else "p") <> "mos" <?=> pm (fst . prettyDelay3) d3)
      ( \(GIMos n lv inp e) ->
          pm pname n <> gpar (ngpadj prettyNetLV lv <.> ngpadj prettyExpr inp <.> ngpadj prettyExpr e)
      )
      l
  MGINIn nin n ds d2 l ->
    prettyItems
      ( (if nin == NITXor then "x" <> pift "n" n <> "or" else pift "n" n <> viaShow nin)
          <?=> prettyDriveStrength ds
          <?=> pm (fst . prettyDelay2) d2
      )
      ( \(GINIn n lv inp) ->
          pm pname n <> gpar (ngpadj prettyNetLV lv <.> padj (cslid1 $ mkng prettyExpr) inp)
      )
      l
  MGINOut r ds d2 l ->
    prettyItems
      ((if r then "not" else "buf") <?=> prettyDriveStrength ds <?=> pm (fst . prettyDelay2) d2)
      ( \(GINOut n lv inp) ->
          pm pname n <> gpar (padj (cslid1 $ mkng prettyNetLV) lv <.> ngpadj prettyExpr inp)
      )
      l
  MGIPassEn r oz d2 l ->
    prettyItems
      (pift "r" r <> "tranif" <> (if oz then "1" else "0") <?=> pm (fst . prettyDelay2) d2)
      ( \(GIPassEn n ll rl e) ->
          pm pname n <> gpar (ngpadj prettyNetLV ll <.> ngpadj prettyNetLV rl <.> ngpadj prettyExpr e)
      )
      l
  MGIPass r l ->
    prettyItems
      (pift "r" r <> "tran")
      (\(GIPass n ll rl) -> pm pname n <> gpar (ngpadj prettyNetLV ll <.> ngpadj prettyNetLV rl))
      l
  MGIPull ud ds l ->
    prettyItems
      ("pull" <> (if ud then "up" else "down") <?=> prettyDriveStrength ds)
      (\(GIPull n lv) -> pm pname n <> gpar (gpadj prettyNetLV lv))
      l
  MGIUDPInst kind ds d2 l ->
    prettyItems
      (rawId kind <?=> prettyDriveStrength ds <?=> pm (fst . prettyDelay2) d2)
      ( \(UDPInst n lv args) ->
          pm pname n <> gpar (gpadj prettyNetLV lv <.> padj (cslid1 $ mkg prettyExpr) args)
      )
      l
  MGIModInst kind param l ->
    prettyItems
      ( rawId kind <?=> case param of
          ParamPositional l -> cslid ("#(" <> softspace) rparen (mkng prettyExpr) l
          ParamNamed l ->
            csl
              ("#(" <> softspace)
              (softline <> rparen)
              (\(Identified i e) -> ng $ dot <> padj prettyIdent i <> gpar (pm (padj prettyMTM) e))
              l
      )
      (\(ModInst n args) -> pname n <> gpar (prettyPortAssign args))
      l
  MGIUnknownInst kind param l ->
    prettyItems
      ( rawId kind <?=> case param of
          Nothing -> mempty
          Just (Left e) -> "#" <> par (padj prettyExpr e)
          Just (Right (e0, e1)) -> "#" <> par (gpadj prettyExpr e0 <.> gpadj prettyExpr e1)
      )
      ( \(UknInst n lv args) ->
          pname n <> gpar (gpadj prettyNetLV lv <.> padj (cslid1 $ mkg prettyExpr) args)
      )
      l
  MGIInitial s -> "initial" <=> prettyAttrStmt protect s
  MGIAlways s -> "always" <=> prettyAttrStmt protect s
  MGILoopGen si vi cond su vu (Identified (Identifier s) i) ->
    let pas i = gpadj $ prettyEq (rawId i) . prettyCExpr
        head = "for" <=> gpar (pas si vi <> semi <+> ngpadj prettyCExpr cond <> semi <+> pas su vu)
     in case fromMGBlockedItem i of
          [Attributed a x] | B.null s ->
            ng $ group head <=> prettyAttr a <?=> prettyModGenSingleItem x protect
          r -> ng head <=> prettyGBlock (Identifier s) r
  MGICondItem ci -> prettyModGenCondItem ci protect -- protect should never be True in practice
  where
    pname (InstanceName i r) = gpadj (padjWith prettyIdent $ pm prettyRange2 r) i
    mauto b = pift "automatic" b
    pidd (NetDecl i d) = padjWith prettyIdent (prettyR2s d) i
    pide (NetInit i e) = prettyEq (rawId i) $ prettyExpr e
    sign b = pift "signed" b
    com (NetProp b vs d3) =
      maybe
        (sign b)
        ( \(vs, r2) ->
            pm (\b -> if b then "vectored" else "scalared") vs <?=> sign b <?=> prettyRange2 r2
        )
        vs
        <?=> pm (fst . prettyDelay3) d3

-- | Nested conditionals with dangling else support
prettyModGenCondItem :: ModGenCondItem -> Bool -> Doc
prettyModGenCondItem ci protect = case ci of
  MGCIIf c t f -> let head = "if" <=> gpar (padj prettyCExpr c) in case f of
    GCBEmpty -> pGCB head protect t <?#> pift "else;" protect
    _ -> pGCB head True t <#> pGCB "else" protect f
  MGCICase c b md ->
    block
      (ng $ "case" <=> gpar (padj prettyCExpr c))
      "endcase"
      $ pl (<#>) (\(GenCaseItem p v) -> pGCB (gpadj (cslid1 prettyCExpr) p <> colon) False v) b
        <?#> case md of GCBEmpty -> mempty; _ -> nest $ pGCB "default:" False md
  where
    isNotCond x = case x of MGICondItem _ -> False; _ -> True
    pGCB head p b = case b of
      GCBEmpty -> head <> semi
      GCBConditional (Attributed a ci) ->
        ng $ group head <=> prettyAttr a <?=> prettyModGenCondItem ci p
      GCBBlock (Identified (Identifier s) r) -> case fromMGBlockedItem r of
        [Attributed a x] | B.null s && isNotCond x ->
          ng $ group head <=> prettyAttr a <?=> prettyModGenSingleItem x protect
        r -> ng head <=> prettyGBlock (Identifier s) r

-- | Generate block
prettyGBlock :: Identifier -> [Attributed ModGenSingleItem] -> Doc
prettyGBlock (Identifier s) =
  block ("begin" <> piff (colon <=> raw s) (B.null s)) "end"
    . pl (<#>) (\(Attributed a x) -> prettyAttr a <?=> prettyModGenSingleItem x False)

prettyGenerateBlock :: GenerateBlock -> Doc
prettyGenerateBlock (Identified s x) = prettyGBlock s $ fromMGBlockedItem x

prettySpecParams :: Maybe Range2 -> NonEmpty SpecParamDecl -> Doc
prettySpecParams rng =
  prettyItemsid
    ("specparam" <?=> pm prettyRange2 rng)
    $ \d -> case d of
      SPDAssign i v -> prettyEq (rawId i) $ prettyCMTM v
      SPDPathPulse io r e ->
        prettyEq
          ("PATHPULSE$" <> pm pPP io)
          (mkid $ par $ ngpadj prettyCMTM r <> piff (comma <+> ngpadj prettyCMTM e) (r == e))
  where
    -- Change to a spaced layout if tools allow it
    pPP (i, o) = ngpadj prettySpecTerm i <> "$" <> fst (prettySpecTerm o)

prettyPathDecl :: SpecPath -> Maybe Bool -> Maybe (Expr, Maybe Bool) -> Doc
prettyPathDecl p pol eds =
  gpar $
    -- maybe edge then source(s)
    ng (pm (pm (\e -> if e then "posedge" else "negedge") . snd) eds <?=> group cin)
      <=> pift po noedge <> (if pf then "=>" else "*>")
      -- destination(s) or '(' destination(s) then maybe polarity ':' data ')'
      <+> ng (maybe cout (\(e, _) -> par $ cout <> po <> colon <+> padj prettyExpr e) eds)
  where
    ppSTs = cslid1 $ mkng prettySpecTerm
    -- polarity
    po = pm (\p -> if p then "+" else "-") pol
    -- edge sensitive path polarity isn't at the same place as non edge sensitive
    noedge = eds == Nothing
    fne = if noedge then uncurry (<>) else \x -> fst x <> newline
    -- parallel or full, source(s), destination(s)
    (pf, (cin, _), cout) = case p of
      SPParallel i o -> (True, prettySpecTerm i, fne $ prettySpecTerm o)
      SPFull i o -> (False, ppSTs i, fne $ ppSTs o)

prettySpecifyItem :: SpecifySingleItem -> Doc
prettySpecifyItem x =
  nest $ case x of
    SISpecParam r l -> prettySpecParams r l
    SIPulsestyleOnevent o -> prettyItemsid "pulsestyle_onevent" prettySpecTerm o
    SIPulsestyleOndetect o -> prettyItemsid "pulsestyle_ondetect" prettySpecTerm o
    SIShowcancelled o -> prettyItemsid "showcancelled" prettySpecTerm o
    SINoshowcancelled o -> prettyItemsid "noshowcancelled" prettySpecTerm o
    SIPathDeclaration mpc p pol eds l ->
      ( case mpc of
          MPCCond e ->
            group $ "if" <=> gpar (padj (prettyGExpr prettyIdent (const mempty) prettyAttr 12) e)
          MPCAlways -> mempty
          MPCNone -> "ifnone"
      )
        <?=> padj (prettyEq (prettyPathDecl p pol eds) . cslid1 prettyCMTM . cPDV) l
        <> semi
    SISetup (STCArgs d r e n) -> "$setup" </> ppA (STCArgs r d e n) <> semi
    SIHold a -> "$hold" </> ppA a <> semi
    SISetupHold a aa -> "$setuphold" </> ppAA a aa <> semi
    SIRecovery a -> "$recovery" </> ppA a <> semi
    SIRemoval a -> "$removal" </> ppA a <> semi
    SIRecrem a aa -> "$recrem" </> ppAA a aa <> semi
    SISkew a -> "$skew" </> ppA a <> semi
    SITimeSkew (STCArgs de re tcl n) meb mra ->
      "$timeskew"
        </> gpar
          ( pTCE re <.> pTCE de
              <.> toc [pexpr tcl, pid n, pm (gpadj prettyCExpr) meb, pm (gpadj prettyCExpr) mra]
          )
        <> semi
    SIFullSkew (STCArgs de re tcl0 n) tcl1 meb mra ->
      "$fullskew"
        </> gpar
          ( pTCE re <.> pTCE de <.> pexpr tcl0
              <.> toc [pexpr tcl1, pid n, pm (gpadj prettyCExpr) meb, pm (gpadj prettyCExpr) mra]
          )
        <> semi
    SIPeriod re tcl n -> "$period" </> par (pCTCE re <.> pexpr tcl <> prid n) <> semi
    SIWidth re tcl mt n ->
      "$width" </> gpar (pCTCE re <.> toc [pexpr tcl, pm (gpadj prettyCExpr) mt, pid n]) <> semi
    SINoChange re de so eo n ->
      "$nochange"
        </> gpar (pTCE re <.> pTCE de <.> ngpadj prettyMTM so <.> ngpadj prettyMTM eo <> prid n)
        <> semi
  where
    toc :: [Doc] -> Doc
    toc = trailoptcat (<.>)
    prid = pm $ \s -> comma <+> padj prettyIdent s
    pid = pm $ padj prettyIdent
    pexpr = gpadj prettyExpr
    pTCC st mbe = case mbe of
      Nothing -> gpadj prettySpecTerm st
      Just (b, e) ->
        group $ group (fst $ prettySpecTerm st) <=> "&&&" <+> pift "~" b <?+> gpadj prettyExpr e
    pTCE (TimingCheckEvent ev st tc) = ng $ pm prettyEdgeDesc ev <?=> pTCC st tc
    pCTCE (ControlledTimingCheckEvent ev st tc) = ng $ prettyEdgeDesc ev <?=> pTCC st tc
    ppA (STCArgs de re tcl n) = gpar $ pTCE re <.> pTCE de <.> pexpr tcl <> prid n
    pIMTM = pm $
      \(Identified i mr) -> ngpadj (padjWith prettyIdent $ pm (group . brk . padj prettyCMTM) mr) i
    ppAA (STCArgs de re tcl0 n) (STCAddArgs tcl1 msc mtc mdr mdd) =
      gpar $
        pTCE re <.> pTCE de <.> pexpr tcl0
          <.> toc
            [ pexpr tcl1,
              pid n,
              pm (ngpadj prettyMTM) msc,
              pm (ngpadj prettyMTM) mtc,
              pIMTM mdr,
              pIMTM mdd
            ]
    cPDV x = case x of
      PDV1 e -> e :|[]
      PDV2 e1 e2 -> [e1, e2]
      PDV3 e1 e2 e3 -> [e1, e2, e3]
      PDV6 e1 e2 e3 e4 e5 e6 -> [e1, e2, e3, e4, e5, e6]
      PDV12 e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11 e12 ->
        [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12]

data ModuleItem'
  = MI'MGI (Attributed ModGenSingleItem)
  | MI'Port [Attribute] Dir SignRange (NonEmpty Identifier)
  | MI'Parameter [Attribute] (ComType ()) (NonEmpty (Identified CMinTypMax))
  | MI'GenReg [Attributed ModGenSingleItem]
  | MI'SpecParam [Attribute] (Maybe Range2) (NonEmpty SpecParamDecl)
  | MI'SpecBlock [SpecifySingleItem]

prettyModuleItems :: [ModuleItem] -> Doc
prettyModuleItems =
  nonEmpty mempty $
    prettyregroup
      (<#>)
      ( \x -> case x of
        MI'MGI (Attributed a i) -> prettyAttr a <?=> prettyModGenSingleItem i False
        MI'Port a d sr l ->
          prettyAttrng a $ prettyItemsid (viaShow d <?=> prettySignRange sr) prettyIdent l
        MI'Parameter a t l -> prettyAttrng a $ prettyXparam "parameter" t l
        MI'GenReg l ->
          block "generate" "endgenerate" $
            pl (<#>) (\(Attributed a i) -> prettyAttr a <?=> prettyModGenSingleItem i False) l
        MI'SpecParam a r l -> prettyAttrng a $ prettySpecParams r l
        MI'SpecBlock l -> block "specify" "endspecify" $ pl (<#>) prettySpecifyItem l
      )
      ( \x -> case x of
        MIMGI i -> MI'MGI $ fromMGBlockedItem1 <$> i
        MIPort (AttrIded a i (d, sr)) -> MI'Port a d sr [i]
        MIParameter (AttrIded a i (Parameter t v)) -> MI'Parameter a t [Identified i v]
        MIGenReg l -> MI'GenReg $ fromMGBlockedItem l
        MISpecParam a r spd -> MI'SpecParam a r [spd]
        MISpecBlock l -> MI'SpecBlock $ fromSpecBlockedItem l
      )
      ( \mi mi' -> case (mi, mi') of
        (MIMGI mgi, MI'MGI l) -> MI'MGI <$> (addAttributed fromMGBlockedItem_add) mgi l
        (MIPort (AttrIded na i (nd, nsr)), MI'Port a d sr l) | na == a && nd == d && nsr == sr ->
          Just $ MI'Port a d sr $ i <| l
        (MIParameter (AttrIded na i (Parameter nt v)), MI'Parameter a t l) | na == a && nt == t ->
          Just $ MI'Parameter a t $ Identified i v <| l
        (MISpecParam na nr spd, MI'SpecParam a r l) | na == a && nr == r ->
          Just $ MI'SpecParam a r $ spd <| l
        _ -> Nothing
      )

prettyPortInter :: [Identified [Identified (Maybe CRangeExpr)]] -> Doc
prettyPortInter =
  cslid mempty mempty $
    \(Identified i@(Identifier ii) l) -> case l of
      [Identified i' x] | i == i' -> first group $ prettySpecTerm (SpecTerm i x)
      _ ->
        if B.null ii
          then portexpr l
          else mkid $ ng $ dot <> padj prettyIdent i <> gpar (padj portexpr l)
  where
    pst (Identified i x) = prettySpecTerm $ SpecTerm i x
    portexpr :: PrettyIdent [Identified (Maybe CRangeExpr)]
    portexpr l = case l of
      [] -> (mempty, mempty)
      [x] -> pst x
      _ -> mkid $ cslid (lbrace <> softspace) rbrace pst l

prettyModuleBlock :: LocalCompDir -> ModuleBlock -> (Doc, LocalCompDir)
prettyModuleBlock (LocalCompDir ts c p dn) (ModuleBlock a i pi b mts mc mp mdn) =
  ( piff (let (a, b) = fromJust mts in "`timescale" <+> tsval a <+> "/" <+> tsval b) (ts == mts)
      <?#> piff (if mc then "`celldefine" else "`endcelldefine") (c == mc)
      <?#> piff
        ( case mp of
            Nothing -> "`nounconnected_drive"
            Just b -> "`unconnected_drive pull" <> if b then "1" else "0"
        )
        (p == mp)
      <?#> piff ("`default_nettype" <+> maybe "none" viaShow mdn) (dn == mdn)
      <?#> block
        ( prettyItem a $
            fpadj (\d -> group $ "module" <=> d) prettyIdent i <> gpar (prettyPortInter pi)
        )
        "endmodule"
        (prettyModuleItems b),
    LocalCompDir mts mc mp mdn
  )
  where
    tsval i =
      let (u, v) = divMod i 3
       in case v of 0 -> "1"; 1 -> "10"; 2 -> "100"
            <> case u of 0 -> "s"; -1 -> "ms"; -2 -> "us"; -3 -> "ns"; -4 -> "ps"; -5 -> "fs"

prettyPrimPorts :: ([Attribute], PrimPort, NonEmpty Identifier) -> Doc
prettyPrimPorts (a, d, l) =
  prettyItem a $
    ( case d of
       PPInput -> "input"
       PPOutput -> "output"
       PPReg -> "reg"
       PPOutReg _ -> "output reg"
    )
    <=> case d of PPOutReg (Just e) -> ids <=> equals <+> gpadj prettyCExpr e; _ -> ids <> s
  where (ids, s) = cslid1 prettyIdent l

-- | Analyses the column size of all lines and returns a list of either
-- | number of successive non-edge columns or the width of a column that contain edges
seqrowAlignment :: NonEmpty SeqRow -> [Either Int Int]
seqrowAlignment =
  maybe [] fst . foldrMapM1
    ( \sr@(SeqRow sri _ _) -> Just (case sri of
        SISeq l0 e l1 -> [Left $ length l0, Right $ edgeprintsize e, Left $ length l1]
        SIComb l -> [Left $ length l],
      totallength sr)
    )
    ( \sr@(SeqRow sri _ _) (sl, tl) ->
        if totallength sr /= tl
          then Nothing
          else case sri of
            SISeq l0 e l1 -> (,) <$> spliceputat (edgeprintsize e) (length l0) sl <*> pure tl
            SIComb l -> Just (sl, tl)
    )
  where
    totallength sr =
      case _srowInput sr of SIComb l -> length l; SISeq l0 e l1 -> 1 + length l0 + length l1
    edgeprintsize x = case x of EdgePos_neg _ -> 1; EdgeDesc _ _ -> 4
    -- Searches and splices a streak of non edge columns and puts an edge column
    spliceputat w n l = case l of
      [] -> Nothing
      Right w' : t ->
        if 0 < n
          then (Right w' :) <$> spliceputat w (n - 1) t
          else Just $ Right (max w w') : t
      Left m : t -> case compare m (n + 1) of
        LT -> (Left m :) <$> spliceputat w (n - m) t
        EQ -> Just $ Left n : Right w : t
        GT -> Just $ Left n : Right w : Left (m - n - 1) : t

-- | Prints levels or edges in an aligned way using analysis results
-- | the second part of the accumulation is Right if there is an edge, Left otherwise
prettySeqIn :: SeqIn -> [Either Int Int] -> Doc
prettySeqIn si =
  fst . foldl'
    ( \(d, si) e -> case (e, si) of
        (Left n, Left l') ->
          let (l0, l1) = splitAt n l'
           in (d <> concat viaShow l0, Left l1)
        (Right w, Left (h : t)) -> (d <> prettylevelwithwidth h w, Left t)
        (Left n, Right (l0, e, l1)) ->
          let (l00, l01) = splitAt n l0
           in (d <> concat viaShow l00, Right (l01, e, l1))
        (Right w, Right ([], e, l1)) ->
          (d <> viaShow e <> pift sp3 (edgeprintsize e < w), Left l1)
        (Right w, Right (h : t, e, l')) -> (d <> prettylevelwithwidth h w, Right (t, e, l'))
    )
    (mempty, case si of SIComb l -> Left $ NE.toList l; SISeq a b c -> Right (a, b, c))
  where
    edgeprintsize x = case x of EdgePos_neg _ -> 1; EdgeDesc _ _ -> 4
    sp3 = raw "   "
    prettylevelwithwidth l w = viaShow l <> pift sp3 (w == 4)
    concat = pl (<>)

-- | Prints a table in a singular block with aligned columns, or just prints it if not possible
prettySeqRows :: NonEmpty SeqRow -> Doc
prettySeqRows l =
  pl (<#>) (case seqrowAlignment l of
    -- fallback prettyprinting because aligned is better but not always possible
    [] -> \(SeqRow si s ns) -> nest $ viaShow si <=> prettyend s ns
    -- aligned prettyprinting
    colws -> \(SeqRow si s ns) -> nest $ prettySeqIn si colws <=> prettyend s ns
  ) l
  where
    prettyend s ns = group (colon <+> viaShow s <=> colon <+> maybe "-" viaShow ns) <> semi

prettyPrimTable :: Doc -> PrimTable -> Doc
prettyPrimTable od b = case b of
  CombTable l ->
    block "table" "endtable" $
      pl
        (<#>)
        (\(CombRow i o) -> nest $ fromString (concatMap show i) <=> colon <+> viaShow o <> semi)
        l
  SeqTable mi l ->
    pm
      ( \iv ->
          nest $
            group ("initial" <=> od)
              <=> equals <+> case iv of { ZOXX -> "1'bx"; ZOXZ -> "0"; ZOXO -> "1" } <> semi
      )
      mi
      <?#> block "table" "endtable" (prettySeqRows l)

prettyPrimitiveBlock :: PrimitiveBlock -> Doc
prettyPrimitiveBlock (PrimitiveBlock a s o i pd b) =
  block
    ( prettyItem a $
        fpadj (\x -> group $ "primitive" <=> x) prettyIdent s
          <> gpar (od <> ol <.> padj (cslid1 prettyIdent) i)
    )
    "endprimitive"
    $ prettyregroup
      (<#>)
      prettyPrimPorts
      (\(AttrIded a i p) -> (a, p, [i]))
      ( \(AttrIded na i np) (a, p, l) -> case (np, p) of
          (PPInput, PPInput) | na == a -> Just (a, p, i <| l)
          _ -> Nothing
      )
      pd
      <#> prettyPrimTable od b
  where
    (od, ol) = prettyIdent o

prettyConfigItem :: ConfigItem -> Doc
prettyConfigItem (ConfigItem ci llu) =
  nest $
    ng
      ( case ci of
          CICell c -> "cell" <=> fst (prettyDot1Ident c)
          CIInst i ->
            "instance" <=> foldrMap1 (fst . prettyIdent) (\a b -> padj prettyIdent a <> dot <> b) i
      )
      <=> ng
        ( case llu of
            LLULiblist ls -> "liblist" <?=> catid prettyBS ls
            LLUUse s c -> "use" <=> padj prettyDot1Ident s <> pift ":config" c
        )
      <> semi
  where
    catid f = foldrMap1' mempty (gpadj f) $ (<=>) . fst . f

prettyVerilog2005 :: Verilog2005 -> Doc
prettyVerilog2005 (Verilog2005 mb pb cb) =
  fst (foldl' (\(d, lcd) m -> first (d <##>) $ prettyModuleBlock lcd m) (mempty, lcdDefault) mb)
    <##> pl (<##>) prettyPrimitiveBlock pb
    <##> pl
      (<##>)
      ( \(ConfigBlock i des b def) ->
          block
            (nest $ "config" <=> padj prettyIdent i <> semi)
            "endconfig"
            $ nest ("design" <?=> catid prettyDot1Ident des) <> semi
              <#> pl (<#>) prettyConfigItem b
              <?#> nest ("default liblist" <?=> catid prettyBS def) <> semi
      )
      cb
  where
    (<##>) = mkopt $ \a b -> a <#> mempty <#> b
    catid f = foldrMap1' mempty (gpadj f) $ (<=>) . fst . f
