-- Module      : Verismith.Verilog2005.PrettyPrinter
-- Description : Pretty printer for the Verilog 2005 AST.
-- Copyright   : (c) 2023 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : experimental
-- Portability : POSIX

module Verismith.Verilog2005.PrettyPrinter
  ( genSource,
    LocalCompDir (..),
    lcdDefault,
  )
where

import Data.Bifunctor (first)
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
  { _LCDTimescale :: Maybe (Int, Int),
    _LCDCell :: Bool,
    _LCDPull :: Maybe Bool,
    _LCDDefNetType :: Maybe NetType
  }

lcdDefault =
  LocalCompDir
    { _LCDTimescale = Nothing,
      _LCDCell = False,
      _LCDPull = Nothing,
      _LCDDefNetType = Just NTWire
    }

genSource :: Maybe Word -> Verilog2005 -> LB.ByteString
genSource mw = layout mw . prettyVerilog2005

(<.>) :: Doc -> Doc -> Doc
(<.>) a b = a <> comma <+> b

brk :: Doc -> Doc
brk = encl lbracket rbracket

brc :: Doc -> Doc
brc = encl lbrace rbrace

par :: Doc -> Doc
par = encl lparen rparen

piff :: Doc -> Bool -> Doc
piff d c = if c then mempty else d

pift :: Doc -> Bool -> Doc
pift d c = if c then d else mempty

prettyXregroup ::
  Foldable f =>
  (Doc -> Doc -> Doc) ->
  (y -> Doc) ->
  (x -> y) ->
  (x -> y -> Maybe y) ->
  f x ->
  Doc
prettyXregroup g c mk f =
  nonEmpty
    mempty
    ( \(h :| t) ->
        uncurry
          collapse
          (foldl (\(d, a) e -> maybe (Just $ collapse d a, mk e) ((,) d) $ f e a) (Nothing, mk h) t)
    )
    . toList
  where
    collapse md a = let d' = c a in maybe d' (g d') md

prettyregroup :: Foldable f => (y -> Doc) -> (x -> y) -> (x -> y -> Maybe y) -> f x -> Doc
prettyregroup = prettyXregroup (<#>)

type PrettyIdent a = Doc -> a -> (Doc, Doc)

psp :: PrettyIdent a -> a -> Doc
psp f = uncurry (<>) . f newline

padj :: PrettyIdent a -> a -> Doc
padj f = uncurry (<>) . f softline

gpsp :: PrettyIdent a -> a -> Doc
gpsp f = uncurry ((<>) . group) . f newline

gpadj :: PrettyIdent a -> a -> Doc
gpadj f = uncurry ((<>) . group) . f softline

ngpadj :: PrettyIdent a -> a -> Doc
ngpadj f = uncurry ((<>) . ng) . f softline

mkg :: PrettyIdent x -> PrettyIdent x
mkg f s = first group . f s

mkng :: PrettyIdent x -> PrettyIdent x
mkng f s = first ng . f s

csl :: Foldable f => Doc -> Doc -> (a -> Doc) -> f a -> Doc
csl = pf $ \a b -> a </> comma <+> b

csl1 :: (a -> Doc) -> NonEmpty a -> Doc
csl1 f (h :| t) = foldl (\b a -> b </> comma <+> f a) (f h) t

rcsl :: Foldable f => Doc -> Doc -> (a -> Doc) -> f a -> Doc
rcsl = pf $ \a b -> b </> comma <+> a

cslid1 :: PrettyIdent a -> PrettyIdent (NonEmpty a)
cslid1 f s = foldrMap1 (f s) $ \a -> first $ \b -> padj f a <.> b

cslid :: Foldable f => Doc -> Doc -> PrettyIdent a -> f a -> Doc
cslid a b f = nonEmpty mempty (\l -> a <> padj (cslid1 f) l <> b) . toList

rcslid1 :: PrettyIdent a -> PrettyIdent (NonEmpty a)
rcslid1 f s (h :| t) = foldl (\b a -> first (\b -> padj f a <.> b) b) (f s h) t

rcslid :: Foldable f => Doc -> Doc -> PrettyIdent a -> f a -> Doc
rcslid a b f = nonEmpty mempty (\l -> a <> padj (rcslid1 f) l <> b) . toList

bcslid1 :: PrettyIdent a -> NonEmpty a -> Doc
bcslid1 f = brc . nest . padj (cslid1 $ mkg f)

pcslid :: Foldable f => PrettyIdent a -> f a -> Doc
pcslid f = cslid (lparen <> softspace) rparen $ mkg f

prettyIdent :: PrettyIdent B.ByteString
prettyIdent s i = (raw i, if B.head i == c2w '\\' then newline else s)

prettyHierIdent :: PrettyIdent HierIdent
prettyHierIdent s (HierIdent p i) =
  first
    ( \i ->
        nest $
          foldr
            ( \(Identified s r) b ->
                ngpadj
                  (pidWith prettyIdent softline $ pm (group . brk . padj prettyCExpr) r)
                  s
                  <> dot
                  <> b
            )
            i
            p
    )
    $ prettyIdent s i

prettyDot1Ident :: PrettyIdent Dot1Ident
prettyDot1Ident s (Dot1Ident h t) =
  maybe (prettyIdent s h) (first (\i -> padj prettyIdent h <> dot <> i) . prettyIdent s) t

pidWith :: PrettyIdent a -> Doc -> Doc -> PrettyIdent a
pidWith f si d so i = if nullDoc d then f so i else (uncurry (<>) (f si i) <> d, so)

pIdentified :: Doc -> (r -> Doc) -> PrettyIdent (Identified r)
pIdentified si f so (Identified i r) = pidWith prettyIdent si (f r) so i

pRanged :: PrettyIdent r -> PrettyIdent (Identified r)
pRanged f = pIdentified softline (group . brk . padj f)

prettySpecTerm :: PrettyIdent SpecTerm
prettySpecTerm = pIdentified softline $ pm prettyCRangeExpr

prettyA :: PrettyIdent Attribute
prettyA s (Identified i e) =
  maybe (prettyIdent s i) (first (\x -> ng $ raw i <=> equals <+> x) . prettyCExpr s) e

prettyAttr :: [Attribute] -> Doc
prettyAttr = nonEmpty mempty (\l -> group $ "(* " <> psp (cslid1 prettyA) l <> "*)")

prettyNumber :: Number -> Doc
prettyNumber x = case x of
  NBinary l -> "b" </> fromString (concatMap show l)
  NOctal l -> "o" </> fromString (concatMap show l)
  NDecimal i -> "d" </> viaShow i
  NHex l -> "h" </> fromString (concatMap show l)
  NXZ b -> if b then "dx" else "dz"

prettyNumIdent :: PrettyIdent NumIdent
prettyNumIdent s x = case x of
  NIIdent i -> prettyIdent s i
  NIReal r -> (raw r, s)
  NINumber n -> (viaShow n, s)

prettyPrim :: PrettyIdent i -> (r -> Doc) -> PrettyIdent (GenPrim i r)
prettyPrim ppid ppr s x = case x of
  PrimNumber Nothing True (NDecimal i) -> (viaShow i, s)
  PrimNumber w b n ->
    ( nest $
        pm (\w -> viaShow w <> softline) w
          <> group ((if b then "'s" else squote) <> prettyNumber n),
      s
    )
  PrimReal r -> (raw r, s)
  PrimIdent i r -> first nest $ pidWith ppid softline (group $ ppr r) s i
  PrimConcat l -> (bcslid1 pexpr l, s)
  PrimMultConcat e l -> (brc $ nest $ gpadj prettyCExpr e <> bcslid1 pexpr l, s)
  PrimFun i a l ->
    pidWith ppid (if null a then softline else space) (prettyAttr a <?=> pcslid pexpr l) s i
  PrimSysFun i l -> first (nest . ("$" <>)) $ pidWith prettyIdent softline (pcslid pexpr l) s i
  PrimMinTypMax m -> (par $ padj (prettyGMTM pexpr) m, s)
  PrimString x -> (raw x, s)
  where
    pexpr = prettyGExpr ppid ppr 12

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

prettyGExpr :: PrettyIdent i -> (r -> Doc) -> Int -> PrettyIdent (GenExpr i r)
prettyGExpr ppid ppr l s e = case e of
  ExprPrim e -> first group $ prettyPrim ppid ppr s e
  ExprUnOp op a e ->
    first
      ( \x ->
          ng $
            viaShow op <> (if null a then mempty else space <> prettyAttr a <> newline) <> x
      )
      $ prettyPrim ppid ppr s e
  ExprBinOp el op a r ->
    let p = preclevel op
        x = psp (pexpr p) el <> viaShow op
        da = prettyAttr a
        pp = pexpr $ p - 1
     in case compare l p of
          LT -> (ng $ par $ x <+> (da <?=> padj pp r), s)
          EQ -> first (\y -> x <+> (da <?=> y)) $ pp s r
          GT -> first (\y -> ng $ x <+> (da <?=> y)) $ pp s r
  ExprCond ec a et ef ->
    let pp s =
          first
            ( \x ->
                nest $
                  group (psp (pexpr 11) ec <> nest ("?" <?+> prettyAttr a))
                    <=> group (psp (pexpr 12) et <> colon <+> x)
            )
            $ pexpr 12 s ef
     in if l < 12 then (ng $ par $ uncurry (<>) $ pp softline, s) else pp s
  where
    pexpr = prettyGExpr ppid ppr

prettyExpr :: PrettyIdent Expr
prettyExpr s (Expr e) = prettyGExpr prettyHierIdent (pm prettyDimRange) 12 s e

prettyCExpr :: PrettyIdent CExpr
prettyCExpr s (CExpr e) = prettyGExpr prettyIdent (pm prettyCRangeExpr) 12 s e

prettyGMTM :: PrettyIdent et -> PrettyIdent (GenMinTypMax et)
prettyGMTM pp s x = case x of
  MTMSingle e -> pp s e
  MTMFull l t h -> first (\x -> gpadj pp l <> colon <-> gpadj pp t <> colon <-> group x) $ pp s h

prettyMTM :: PrettyIdent MinTypMax
prettyMTM = prettyGMTM prettyExpr

prettyCMTM :: PrettyIdent CMinTypMax
prettyCMTM = prettyGMTM prettyCExpr

prettyRange2 :: Range2 -> Doc
prettyRange2 (Range2 m l) = brk $ gpadj prettyCExpr m <> colon <-> gpadj prettyCExpr l

prettyR2s :: [Range2] -> Doc
prettyR2s = pf (</>) mempty mempty $ group . prettyRange2

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
prettyDimRange (DimRange dr) = prettyGDR prettyExpr dr

prettyCDimRange :: CDimRange -> Doc
prettyCDimRange (CDimRange dr) = prettyGDR prettyCExpr dr

prettySignRange :: SignRange -> Doc
prettySignRange (SignRange s r) = pift "signed" s <?=> pm (group . prettyRange2) r

prettyFPT :: FunParType -> Doc
prettyFPT x = case x of FPTComType ct -> viaShow ct; FPTSignRange sr -> prettySignRange sr

prettyDriveStrength :: DriveStrength -> Doc
prettyDriveStrength x = case x of
  DSNormal StrStrong StrStrong -> mempty
  DSNormal s0 s1 -> par $ viaShow s0 <> "0" </> comma <+> viaShow s1 <> "1"
  DSHighZ False s -> par $ "highz0" </> comma <+> viaShow s <> "1"
  DSHighZ True s -> par $ viaShow s <> "0" </> comma <+> "highz1"

prettyDelay3 :: Delay3 -> Doc
prettyDelay3 x =
  "#" <> case x of
    D3Base ni -> fst $ prettyNumIdent mempty ni
    D31 m -> par $ padj prettyMTM m
    D32 m1 m2 -> par $ ngpadj prettyMTM m1 <.> ngpadj prettyMTM m2
    D33 m1 m2 m3 -> par $ ngpadj prettyMTM m1 <.> ngpadj prettyMTM m2 <.> ngpadj prettyMTM m3

prettyDelay2 :: Delay2 -> Doc
prettyDelay2 x =
  "#" <> case x of
    D2Base ni -> fst $ prettyNumIdent mempty ni
    D21 m -> par $ padj prettyMTM m
    D22 m1 m2 -> par $ ngpadj prettyMTM m1 <.> ngpadj prettyMTM m2

prettyDelay1 :: Delay1 -> Doc
prettyDelay1 x =
  "#" <> case x of
    D1Base ni -> fst $ prettyNumIdent mempty ni
    D11 m -> par $ padj prettyMTM m

prettyLValue :: (dr -> Doc) -> PrettyIdent (LValue dr)
prettyLValue f s x = case x of
  LVSingle hi r -> first nest $ pidWith prettyHierIdent softline (pm f r) s hi
  LVConcat l -> (bcslid1 (prettyLValue f) l, s)

prettyNetLV :: PrettyIdent NetLValue
prettyNetLV = prettyLValue $ group . prettyCDimRange

prettyVarLV :: PrettyIdent VarLValue
prettyVarLV = prettyLValue $ group . prettyDimRange

prettyNetAssign :: PrettyIdent NetAssign
prettyNetAssign s (NetAssign l e) =
  first (\x -> nest $ psp prettyNetLV l <> equals <+> group x) $ prettyExpr s e

prettyVarAssign :: PrettyIdent VarAssign
prettyVarAssign s (VarAssign l e) =
  first (\x -> nest $ psp prettyVarLV l <> equals <+> group x) $ prettyExpr s e

prettyEventControl :: EventControl -> Doc
prettyEventControl x =
  "@" <> case x of
    ECDeps -> "*"
    ECIdent hi -> nest $ fst $ prettyHierIdent mempty hi
    ECExpr l ->
      par $
        padj
          ( cslid1 $ \s (EventPrim p e) ->
              first
                (\x -> ng $ case p of { EPAny -> mempty; EPPos -> "posedge"; EPNeg -> "negedge" } <?=> group x)
                $ prettyExpr s e
          )
          l

prettyStatement :: Statement -> Doc
prettyStatement x = case x of
  SBlockAssign b (VarAssign lv v) dec ->
    nest $
      gpsp prettyVarLV lv
        <> group
          ( piff langle b <> equals
              <+> pm
                ( \x -> case x of
                    DECRepeat e ev -> group ("repeat" <=> par (padj prettyExpr e)) <=> prettyEventControl ev
                    DECDelay d -> prettyDelay1 d
                    DECEvent e -> prettyEventControl e
                )
                dec
              <=> gpadj prettyExpr v
          )
        <> semi
  SProcContAssign pca -> nest $
    (<> semi) $ case pca of
      PCAAssign va -> "assign" <=> padj prettyVarAssign va
      PCADeassign lv -> "deassign" <=> padj prettyVarLV lv
      PCAForce lv -> "force" <=> either (padj prettyVarAssign) (padj prettyNetAssign) lv
      PCARelease lv -> "release" <=> either (padj prettyVarLV) (padj prettyNetLV) lv
  SCase zox e b s ->
    block
      ( nest $
          "case" <> case zox of ZOXZ -> "z"; ZOXO -> mempty; ZOXX -> "x"
            <=> group (par $ padj prettyExpr e)
      )
      "endcase"
      $ pf
        (flip (<#>))
        mempty
        mempty
        ( \(CaseItem p v) ->
            gpadj (cslid1 prettyExpr) p <> colon <+> ng (prettyMybStmt v)
        )
        b
        <?#> piff (nest $ "default:" <?=> prettyMybStmt s) (s == Attributed [] Nothing)
  SIf c t f ->
    let head = "if" <=> group (par $ padj prettyExpr c)
     in (if stmtDanglingElse f t then block (ng head <=> "begin") "end" else ng . (group head <=>))
          (prettyMybStmt t)
          <?#> case f of
            Attributed [] Nothing -> mempty
            Attributed _ (Just (SIf _ _ _)) -> group $ "else" <=> prettyMybStmt f
            _ -> ng $ "else" <=> prettyMybStmt f
  SDisable hi -> nest $ "disable" <=> padj prettyHierIdent hi <> semi
  SProcTimingControl c s ->
    nest $ group (either prettyDelay1 prettyEventControl c) <=> prettyMybStmt s
  SEventTrigger hi e ->
    nest $
      "->" <+> padj prettyHierIdent hi
        <> pf (</>) mempty mempty (group . brk . padj prettyExpr) e
        <> semi
  SWait e s -> nest $ group ("wait" <=> par (padj prettyExpr e)) <=> prettyMybStmt s
  SLoop ls s -> nest $
    (<=> prettyAttrStmt s) $ case ls of
      LSForever -> "forever"
      LSRepeat e -> ng $ "repeat" <=> par (padj prettyExpr e)
      LSWhile e -> ng $ "while" <=> par (padj prettyExpr e)
      LSFor i c u ->
        ng $
          "for"
            <=> par
              (gpadj prettyVarAssign i <> semi <+> gpadj prettyExpr c <> semi <+> gpadj prettyVarAssign u)
  SBlock h ps s ->
    block
      (nest $ (if ps then "fork" else "begin") <?/> pm (\(s, _, _, _) -> colon <+> raw s) h)
      (if ps then "join" else "end")
      $ maybe mempty (\(_, p, lp, d) -> prettyParamStd p lp d) h
        <?#> pf (<#>) mempty mempty prettyAttrStmt s
  SSysTaskEnable s a ->
    nest $ "$" <> padj prettyIdent s <> pcslid (\s -> maybe (mempty, s) $ prettyExpr s) a <> semi
  STaskEnable hi a -> nest $ padj prettyHierIdent hi <> pcslid prettyExpr a <> semi

prettyAttrStmt :: AttrStmt -> Doc
prettyAttrStmt (Attributed a s) = prettyAttr a <?=> prettyStatement s

prettyMybStmt :: MybStmt -> Doc
prettyMybStmt (Attributed a s) = prettyAttr a <?=> maybe semi prettyStatement s

data GateInst'
  = GI'CMos Bool (Maybe Delay3) (NonEmpty (Identified (Maybe Range2, NetLValue, Expr, Expr, Expr)))
  | GI'Enable
      Bool
      Bool
      DriveStrength
      (Maybe Delay3)
      (NonEmpty (Identified (Maybe Range2, NetLValue, Expr, Expr)))
  | GI'Mos Bool Bool (Maybe Delay3) (NonEmpty (Identified (Maybe Range2, NetLValue, Expr, Expr)))
  | GI'NIn
      NInputType
      Bool
      DriveStrength
      (Maybe Delay2)
      (NonEmpty (Identified (Maybe Range2, NetLValue, NonEmpty Expr)))
  | GI'NOut
      Bool
      DriveStrength
      (Maybe Delay2)
      (NonEmpty (Identified (Maybe Range2, NonEmpty NetLValue, Expr)))
  | GI'PassEn
      Bool
      Bool
      (Maybe Delay2)
      (NonEmpty (Identified (Maybe Range2, NetLValue, NetLValue, Expr)))
  | GI'Pass Bool (NonEmpty (Identified (Maybe Range2, NetLValue, NetLValue)))
  | GI'Pull Bool DriveStrength (NonEmpty (Identified (Maybe Range2, NetLValue)))

mkgi' :: B.ByteString -> Maybe Range2 -> GateInst -> GateInst'
mkgi' s rng gi = case gi of
  GICMos r d3 lv inp nc pc -> GI'CMos r d3 $ Identified s (rng, lv, inp, nc, pc) :| []
  GIEnable r oz ds d3 lv inp en -> GI'Enable r oz ds d3 $ Identified s (rng, lv, inp, en) :| []
  GIMos r np d3 lv inp en -> GI'Mos r np d3 $ Identified s (rng, lv, inp, en) :| []
  GINIn nin n ds d2 lv inp -> GI'NIn nin n ds d2 $ Identified s (rng, lv, inp) :| []
  GINOut r ds d2 lv inp -> GI'NOut r ds d2 $ Identified s (rng, lv, inp) :| []
  GIPassEn r oz d2 llv rlv en -> GI'PassEn r oz d2 $ Identified s (rng, llv, rlv, en) :| []
  GIPass r llv rlv -> GI'Pass r $ Identified s (rng, llv, rlv) :| []
  GIPull ud ds lv -> GI'Pull ud ds $ Identified s (rng, lv) :| []

ppGI' :: GateInst' -> Doc
ppGI' x = case x of
  GI'CMos r d3 l ->
    group (pift "r" r <> "cmos" <?=> pm prettyDelay3 d3)
      <=> group
        ( csl1
            ( \(Identified s (rng, lv, inp, nc, pc)) ->
                ng $
                  pidr s rng
                    <> group
                      ( par $
                          ngpadj prettyNetLV lv <.> ngpadj prettyExpr inp
                            <.> ngpadj prettyExpr nc
                            <.> ngpadj prettyExpr pc
                      )
            )
            l
        )
  GI'Enable r oz ds d3 l ->
    group
      ( (if r then "not" else "buf") <> "if" <> (if oz then "1" else "0")
          <?=> prettyDriveStrength ds
          <?=> pm prettyDelay3 d3
      )
      <=> group
        ( csl1
            ( \(Identified s (rng, lv, inp, en)) ->
                ng $
                  pidr s rng
                    <> group
                      ( par $
                          ngpadj prettyNetLV lv <.> ngpadj prettyExpr inp <.> ngpadj prettyExpr en
                      )
            )
            l
        )
  GI'Mos r np d3 l ->
    group (pift "r" r <> (if np then "n" else "p") <> "mos" <?=> pm prettyDelay3 d3)
      <=> group
        ( csl1
            ( \(Identified s (rng, lv, inp, en)) ->
                ng $
                  pidr s rng
                    <> group
                      ( par $
                          ngpadj prettyNetLV lv <.> ngpadj prettyExpr inp <.> ngpadj prettyExpr en
                      )
            )
            l
        )
  GI'NIn nin n ds d2 l ->
    group
      ( (if nin == NITXor then "x" <> pift "n" n <> "or" else pift "n" n <> viaShow nin)
          <?=> prettyDriveStrength ds
          <?=> pm prettyDelay2 d2
      )
      <=> group
        ( csl1
            ( \(Identified s (rng, lv, inp)) ->
                ng $
                  pidr s rng
                    <> group
                      ( par $
                          ngpadj prettyNetLV lv <.> padj (cslid1 $ \s -> first ng . prettyExpr s) inp
                      )
            )
            l
        )
  GI'NOut r ds d2 l ->
    group ((if r then "not" else "buf") <?=> prettyDriveStrength ds <?=> pm prettyDelay2 d2)
      <=> group
        ( csl1
            ( \(Identified s (rng, lv, inp)) ->
                ng $
                  pidr s rng
                    <> group
                      ( par $
                          padj (cslid1 $ \s -> first ng . prettyNetLV s) lv <.> ngpadj prettyExpr inp
                      )
            )
            l
        )
  GI'PassEn r oz d2 l ->
    group (pift "r" r <> "tranif" <> (if oz then "1" else "0") <?=> pm prettyDelay2 d2)
      <=> group
        ( csl1
            ( \(Identified s (rng, llv, rlv, en)) ->
                ng $
                  pidr s rng
                    <> group
                      ( par $
                          ngpadj prettyNetLV llv <.> ngpadj prettyNetLV rlv <.> ngpadj prettyExpr en
                      )
            )
            l
        )
  GI'Pass r l ->
    pift "r" r <> "tran"
      <=> group
        ( csl1
            ( \(Identified s (rng, llv, rlv)) ->
                ng $
                  pidr s rng
                    <> group
                      ( par $
                          ngpadj prettyNetLV llv <.> ngpadj prettyNetLV rlv
                      )
            )
            l
        )
  GI'Pull ud ds l ->
    group ("pull" <> (if ud then "up" else "down") <?=> prettyDriveStrength ds)
      <=> group
        ( csl1
            ( \(Identified s (rng, lv)) ->
                ng $
                  pidr s rng
                    <> group
                      ( par $
                          gpadj prettyNetLV lv
                      )
            )
            l
        )
  where
    pidr s r =
      piff
        ( let i = padj prettyIdent s
           in maybe i (\r -> group (i <> prettyRange2 r) <> newline) r
        )
        $ B.null s

prettyGateInsts :: Attributed (NonEmpty (Identified (Maybe Range2, GateInst))) -> Doc
prettyGateInsts (Attributed a l) =
  prettyregroup
    (\x -> prettyAttr a <?=> nest (ppGI' x) <> semi)
    (\(Identified s (rng, gi)) -> mkgi' s rng gi)
    ( \(Identified s (rng, gi)) gi' -> case (gi', gi) of
        (GI'CMos r' d3' l, GICMos r d3 lv inp nc pc)
          | r == r' && d3 == d3' ->
            Just $ GI'CMos r' d3' $ Identified s (rng, lv, inp, nc, pc) <| l
        (GI'Enable r' oz' ds' d3' l, GIEnable r oz ds d3 lv inp en)
          | r == r' && oz == oz' && ds == ds' && d3 == d3' ->
            Just $ GI'Enable r' oz' ds' d3' $ Identified s (rng, lv, inp, en) <| l
        (GI'Mos r' np' d3' l, GIMos r np d3 lv inp en)
          | r == r' && np == np' && d3 == d3' ->
            Just $ GI'Mos r' np' d3' $ Identified s (rng, lv, inp, en) <| l
        (GI'NIn nin' n' ds' d2' l, GINIn nin n ds d2 lv inp)
          | nin == nin' && n == n' && ds == ds' && d2 == d2' ->
            Just $ GI'NIn nin' n' ds' d2' $ Identified s (rng, lv, inp) <| l
        (GI'NOut r' ds' d2' l, GINOut r ds d2 lv inp)
          | r == r' && ds == ds' && d2 == d2' ->
            Just $ GI'NOut r' ds' d2' $ Identified s (rng, lv, inp) <| l
        (GI'PassEn r' oz' d2' l, GIPassEn r oz d2 llv rlv en)
          | r == r' && oz == oz' && d2 == d2' ->
            Just $ GI'PassEn r' oz' d2' $ Identified s (rng, llv, rlv, en) <| l
        (GI'Pass r' l, GIPass r llv rlv)
          | r == r' ->
            Just $ GI'Pass r' $ Identified s (rng, llv, rlv) <| l
        (GI'Pull ud' ds' l, GIPull ud ds lv)
          | ud == ud' && ds == ds' ->
            Just $ GI'Pull ud' ds' $ Identified s (rng, lv) <| l
        _ -> Nothing
    )
    l

prettyEdgeDesc :: EdgeDesc -> Doc
prettyEdgeDesc x =
  if x == V.fromList [True, True, False, False, False, False, False, True, False, False]
    then "posedge"
    else
      if x == V.fromList [False, False, False, True, True, False, True, False, False, False]
        then "negedge"
        else
          group $
            "edge"
              <=> brk
                ( csl mempty mempty raw $
                    V.ifoldl'
                      ( \l i b ->
                          if b
                            then
                              ( case i of
                                  0 -> "01"
                                  1 -> "0x"
                                  2 -> "0z"
                                  3 -> "10"
                                  4 -> "1x"
                                  5 -> "1z"
                                  6 -> "x0"
                                  7 -> "x1"
                                  8 -> "z0"
                                  9 -> "z1"
                              ) :
                              l
                            else l
                      )
                      []
                      x
                )

prettySTC :: SystemTimingCheck -> Doc
prettySTC x = case x of
  STCSetup (STCArgs d r e n) -> "$setup" </> ppA (STCArgs r d e n)
  STCHold a -> "$hold" </> ppA a
  STCSetupHold a aa -> "$setuphold" </> ppAA a aa
  STCRecovery a -> "$recovery" </> ppA a
  STCRemoval a -> "$removal" </> ppA a
  STCRecrem a aa -> "$recrem" </> ppAA a aa
  STCSkew a -> "$skew" </> ppA a
  STCTimeSkew (STCArgs de re tcl n) meb mra ->
    "$timeskew"
      </> par
        ( pTCE re <.> pTCE de
            <.> trailoptcat (<.>) [pexpr tcl, pid n, pm (gpadj prettyCExpr) meb, pm (gpadj prettyCExpr) mra]
        )
  STCFullSkew (STCArgs de re tcl0 n) tcl1 meb mra ->
    "$fullskew"
      </> par
        ( pTCE re <.> pTCE de <.> pexpr tcl0
            <.> trailoptcat
              (<.>)
              [pexpr tcl1, pid n, pm (gpadj prettyCExpr) meb, pm (gpadj prettyCExpr) mra]
        )
  STCPeriod re tcl n -> "$period" </> par (pCTCE re <.> pexpr tcl <> prid n)
  STCWidth re tcl mt n ->
    "$width"
      </> par
        (pCTCE re <.> trailoptcat (<.>) [pexpr tcl, pm (gpadj prettyCExpr) mt, pid n])
  STCNoChange re de so eo n ->
    "$nochange"
      </> par
        (pTCE re <.> pTCE de <.> ngpadj prettyMTM so <.> ngpadj prettyMTM eo <> prid n)
  where
    prid s = piff (comma <+> padj prettyIdent s) $ B.null s
    pid s = piff (padj prettyIdent s) $ B.null s
    pexpr = gpadj prettyExpr
    pTCC (b, e) = "&&&" <+> pift "~" b <?+> gpadj prettyExpr e
    pTCE (TimingCheckEvent ev st tc) =
      ng $ pm prettyEdgeDesc ev <?=> gpadj (pidWith prettySpecTerm newline $ pm pTCC tc) st
    pCTCE (ControlledTimingCheckEvent ev st tc) =
      ng $ prettyEdgeDesc ev <?=> gpadj (pidWith prettySpecTerm newline $ pm pTCC tc) st
    ppA (STCArgs de re tcl n) = par $ pTCE re <.> pTCE de <.> pexpr tcl <> prid n
    pIMTM (Identified i mr) =
      ngpadj (pidWith prettyIdent softline $ pm (group . brk . padj prettyCMTM) mr) i
    ppAA (STCArgs de re tcl0 n) (STCAddArgs tcl1 msc mtc mdr mdd) =
      par $
        pTCE re <.> pTCE de <.> pexpr tcl0
          <.> trailoptcat
            (<.>)
            [ pexpr tcl1,
              pid n,
              pm (ngpadj prettyMTM) msc,
              pm (ngpadj prettyMTM) mtc,
              pIMTM mdr,
              pIMTM mdd
            ]

data SpecifyItem'
  = SI'Pulsestyle Bool (NonEmpty SpecTerm)
  | SI'Showcancelled Bool (NonEmpty SpecTerm)
  | SI'SystemTimingCheck SystemTimingCheck
  | SI'PathDecl ModulePathCondition SpecPath (Maybe Bool) (Maybe (Expr, Maybe Bool)) (NonEmpty CMinTypMax)

prettySpecifyItems :: [SpecifyItem] -> Doc
prettySpecifyItems =
  prettyregroup
    ( \x -> nest $
        (<> semi) $ case x of
          SI'Pulsestyle ed o -> "pulsestyle_on" <> (if ed then "event" else "detect") <=> padj rppSTs o
          SI'Showcancelled c o -> piff "no" c <> "showcancelled" <=> padj rppSTs o
          SI'PathDecl cond conn pol edep v ->
            let po = pm (\p -> if p then "+" else "-") pol
                nedge = edep == Nothing
                (pf, (cin, _), cout) = case conn of
                  SPParallel i o ->
                    ( True,
                      prettySpecTerm newline i,
                      uncurry (<>) $ prettySpecTerm (if nedge then softline else newline) o
                    )
                  SPFull i o ->
                    ( False,
                      ppSTs newline i,
                      uncurry (<>) $ ppSTs (if nedge then softline else newline) o
                    )
             in nest $
                  ( case cond of
                      MPCCond e -> group $ "if" <=> par (padj (prettyGExpr prettyIdent (const mempty) 12) e)
                      MPCAlways -> mempty
                      MPCNone -> "ifnone"
                  )
                    <?=> ng
                      ( group
                          ( par $
                              ng (pm (pm (\e -> if e then "posedge" else "negedge") . snd) edep <?=> group cin)
                                <=> pift po nedge <> (if pf then "=>" else "*>")
                                <+> ng (maybe cout (\(e, _) -> par $ cout <> po <> colon <+> padj prettyExpr e) edep)
                          )
                          <=> equals <+> group (padj (cslid1 $ \s -> first ng . prettyCMTM s) v)
                      )
          SI'SystemTimingCheck stc -> prettySTC stc
    )
    ( \si -> case si of
        SIPulsestyle b s -> SI'Pulsestyle b $ s :| []
        SIShowcancelled b s -> SI'Showcancelled b $ s :| []
        SISystemTimingCheck s -> SI'SystemTimingCheck s
        SIPathDeclaration mpd sp mb meb v -> SI'PathDecl mpd sp mb meb v
    )
    ( \si si' -> case (si', si) of
        (SI'Pulsestyle b' s', SIPulsestyle b s) | b == b' -> Just $ SI'Pulsestyle b' $ s <| s'
        (SI'Showcancelled b' s', SIShowcancelled b s) | b == b' -> Just $ SI'Showcancelled b' $ s <| s'
        _ -> Nothing
    )
  where
    ppSTs = cslid1 $ mkng prettySpecTerm
    rppSTs = rcslid1 $ mkng prettySpecTerm

prettyEDI :: Doc -> Either [Range2] CExpr -> (Doc, Doc, Doc)
prettyEDI s x = case x of
  Left r2 -> (softline, group $ prettyR2s r2, s)
  Right ce -> let (x, d) = prettyCExpr s ce in (newline, equals <+> group x, d)

data BlockDecl' t
  = BD'Reg SignRange (NonEmpty (Identified t))
  | BD'Int (NonEmpty (Identified t))
  | BD'Real (NonEmpty (Identified t))
  | BD'Time (NonEmpty (Identified t))
  | BD'RealTime (NonEmpty (Identified t))
  | BD'Event (NonEmpty (Identified [Range2]))

mkbd' :: B.ByteString -> BlockDecl t -> BlockDecl' t
mkbd' s bd = case bd of
  BDReg sr x -> BD'Reg sr $ Identified s x :| []
  BDInt x -> BD'Int $ Identified s x :| []
  BDReal x -> BD'Real $ Identified s x :| []
  BDTime x -> BD'Time $ Identified s x :| []
  BDRealTime x -> BD'RealTime $ Identified s x :| []
  BDEvent r2 -> BD'Event $ Identified s r2 :| []

prettyBD' :: (Doc -> t -> (Doc, Doc, Doc)) -> BlockDecl' t -> Doc
prettyBD' f x = nest $
  (<> semi) $ case x of
    BD'Reg sr l -> group ("reg" <?=> prettySignRange sr) <=> pl' l
    BD'Int l -> "integer" <=> pl' l
    BD'Real l -> "real" <=> pl' l
    BD'Time l -> "time" <=> pl' l
    BD'RealTime l -> "realtime" <=> pl' l
    BD'Event l -> "event" <=> gpadj (cslid1 $ pIdentified softline prettyR2s) l
  where
    pl' = gpadj $
      cslid1 $ \s (Identified i x) ->
        let (si, d, so) = f s x in mkg (pidWith prettyIdent si d) so i

prettyMGDBlockDecl :: Attributed (NonEmpty (Identified (BlockDecl (Either [Range2] CExpr)))) -> Doc
prettyMGDBlockDecl (Attributed a l) =
  prettyregroup
    (\x -> prettyAttr a <?=> prettyBD' prettyEDI x)
    (\(Identified s bd) -> mkbd' s bd)
    ( \(Identified s bd) bd' -> case (bd', bd) of
        (BD'Reg sr' l, BDReg sr x) | sr == sr' -> Just $ BD'Reg sr' $ Identified s x <| l
        (BD'Int l, BDInt x) -> Just $ BD'Int $ Identified s x <| l
        (BD'Real l, BDReal x) -> Just $ BD'Real $ Identified s x <| l
        (BD'Time l, BDTime x) -> Just $ BD'Time $ Identified s x <| l
        (BD'RealTime l, BDRealTime x) -> Just $ BD'RealTime $ Identified s x <| l
        (BD'Event l, BDEvent r2) -> Just $ BD'Event $ Identified s r2 <| l
        _ -> Nothing
    )
    l

prettyStdBlockDecl :: StdBlockDecl -> Doc
prettyStdBlockDecl =
  prettyregroup
    (\(Attributed a bd') -> prettyAttr a <?=> prettyBD' (\s _ -> (mempty, mempty, s)) bd')
    (\(AttrIded a s bd) -> Attributed a $ mkbd' s bd)
    ( \(AttrIded na s bd) (Attributed a bd') ->
        if na /= a
          then Nothing
          else case (bd', bd) of
            (BD'Reg sr' l, BDReg sr x)
              | sr == sr' ->
                Just $ Attributed a $ BD'Reg sr' $ Identified s x <| l
            (BD'Int l, BDInt x) -> Just $ Attributed a $ BD'Int $ Identified s x <| l
            (BD'Real l, BDReal x) -> Just $ Attributed a $ BD'Real $ Identified s x <| l
            (BD'Time l, BDTime x) -> Just $ Attributed a $ BD'Time $ Identified s x <| l
            (BD'RealTime l, BDRealTime x) -> Just $ Attributed a $ BD'RealTime $ Identified s x <| l
            (BD'Event l, BDEvent r2) -> Just $ Attributed a $ BD'Event $ Identified s r2 <| l
            _ -> Nothing
    )

data NetKind'
  = NK'NetA NetType DriveStrength (NonEmpty (Identified Expr))
  | NK'NetD NetType (NonEmpty (Identified [Range2]))
  | NK'TriA DriveStrength (NonEmpty (Identified Expr))
  | NK'TriC ChargeStrength (NonEmpty (Identified [Range2]))

prettyNetDecls ::
  Attributed (Bool, Maybe (Maybe Bool, Range2), Maybe Delay3, NonEmpty (Identified NetKind)) ->
  Doc
prettyNetDecls (Attributed a (b, vs, d3, l)) =
  prettyregroup
    ( \nk' ->
        prettyAttr a
          <?=> ng
            ( (<> semi) $ case nk' of
                NK'NetA nt ds l -> group (viaShow nt <?=> prettyDriveStrength ds <?=> com) <?=> pide l
                NK'NetD nt l -> group (viaShow nt <?=> com) <?=> pidd l
                NK'TriA ds l -> group ("trireg" <?=> prettyDriveStrength ds <?=> com) <?=> pide l
                NK'TriC cs l -> group ("trireg" <?=> piff (viaShow cs) (cs == CSMedium) <?=> com) <?=> pidd l
            )
    )
    ( \(Identified s nk) -> case nk of
        NKNet nt edsi -> case edsi of
          Left r2 -> NK'NetD nt $ Identified s r2 :| []
          Right (ds, e) -> NK'NetA nt ds $ Identified s e :| []
        NKTriD ds e -> NK'TriA ds $ Identified s e :| []
        NKTriC cs r2 -> NK'TriC cs $ Identified s r2 :| []
    )
    ( \(Identified s nk) nk' -> case (nk', nk) of
        (NK'NetD nt' l, NKNet nt (Left r2))
          | nt == nt' ->
            Just $ NK'NetD nt' $ Identified s r2 <| l
        (NK'NetA nt' ds' l, NKNet nt (Right (ds, e)))
          | nt == nt' && ds == ds' ->
            Just $ NK'NetA nt' ds' $ Identified s e <| l
        (NK'TriA ds' l, NKTriD ds e) | ds == ds' -> Just $ NK'TriA ds' $ Identified s e <| l
        (NK'TriC cs' l, NKTriC cs r2) | cs == cs' -> Just $ NK'TriC cs' $ Identified s r2 <| l
        _ -> Nothing
    )
    l
  where
    pidd = gpadj $ cslid1 $ \s -> first ng . pIdentified softline prettyR2s s
    pide = gpadj $
      cslid1 $ \s (Identified i e) ->
        first (\x -> ng $ raw i <=> equals <+> x) $ prettyExpr s e
    sign = pift "signed" b
    com =
      maybe
        sign
        ( \(vs, r2) ->
            pm (\b -> if b then "vectored" else "scalared") vs <?=> sign <?=> prettyRange2 r2
        )
        vs
        <?=> pm prettyDelay3 d3

prettyXparam :: Doc -> [Parameter] -> Doc
prettyXparam pre =
  prettyregroup
    ( \(Attributed a (t, l)) ->
        prettyAttr a
          <?=> ng
            ( group (pre <=> prettyFPT t)
                <?=> padj
                  ( cslid1 $
                      \s (i, v) -> first (\x -> ng $ raw i <=> equals <+> group x) $ prettyCMTM s v
                  )
                  l
            )
          <> semi
    )
    (\(Parameter a s t v) -> Attributed a (t, (s, v) :| []))
    ( \(Parameter na s nt v) (Attributed a (t, l)) ->
        if na == a && nt == t then Just $ Attributed a (t, (s, v) <| l) else Nothing
    )

prettyParamStd :: [Parameter] -> [Parameter] -> StdBlockDecl -> Doc
prettyParamStd p lp d =
  prettyXparam "parameter" p <?#> prettyXparam "localparam" lp <?#> prettyStdBlockDecl d

data ModGenDecl'
  = MGD'BlockDecl (NonEmpty (Identified (BlockDecl (Either [Range2] CExpr))))
  | MGD'Net Bool (Maybe (Maybe Bool, Range2)) (Maybe Delay3) (NonEmpty (Identified NetKind))
  | MGD'GenVar (NonEmpty B.ByteString)
  | MGD'Task
      Bool
      B.ByteString
      [AttrIded (Dir, TaskFunType)]
      [Parameter]
      [Parameter]
      StdBlockDecl
      MybStmt
  | MGD'Func
      Bool
      (Maybe FunParType)
      B.ByteString
      [AttrIded TaskFunType]
      [Parameter]
      [Parameter]
      StdBlockDecl
      Statement

prettyModGenDecls :: [AttrIded ModGenDecl] -> Doc
prettyModGenDecls =
  prettyregroup
    ( \(Attributed a x) -> case x of
        MGD'BlockDecl l -> prettyMGDBlockDecl $ Attributed a $ NE.reverse l
        MGD'Net b v d l -> prettyNetDecls $ Attributed a (b, v, d, NE.reverse l)
        MGD'GenVar l -> prettyAttr a <?=> ng ("genvar" <=> padj (cslid1 prettyIdent) l) <> semi
        MGD'Task aut s port param lp d b ->
          prettyAttr a
            <?=> block
              ( group ("task" <?=> mauto aut)
                  <=> ng
                    ( padj prettyIdent s
                        <> group
                          ( par $
                              prettyXregroup
                                (<.>)
                                pPort
                                (\(AttrIded a s (d, t)) -> Attributed a (d, t, s :| []))
                                ( \(AttrIded na s (nd, nt)) (Attributed a (d, t, l)) ->
                                    if na == a && nd == d && nt == t
                                      then Just $ Attributed a (d, t, s <| l)
                                      else Nothing
                                )
                                port
                          )
                    )
                    <> semi
              )
              "endtask"
              (prettyParamStd param lp d <?#> prettyMybStmt b)
        MGD'Func aut t s port param lp d b ->
          prettyAttr a
            <?=> block
              ( group ("function" <?=> mauto aut <?=> pm prettyFPT t)
                  <=> ng
                    ( padj prettyIdent s
                        <> group
                          ( par $
                              prettyXregroup
                                (<.>)
                                (\(Attributed a (t, l)) -> pPort $ Attributed a (DirIn, t, l))
                                (\(AttrIded a s t) -> Attributed a (t, s :| []))
                                ( \(AttrIded na s nt) (Attributed a (t, l)) ->
                                    if na /= a || nt /= t
                                      then Nothing
                                      else Just $ Attributed a (t, s <| l)
                                )
                                port
                          )
                    )
                    <> semi
              )
              "endfunction"
              (prettyParamStd param lp d <?#> prettyStatement b)
    )
    ( \(AttrIded a s mgd) -> Attributed a $ case mgd of
        MGDBlockDecl bd -> MGD'BlockDecl $ Identified s bd :| []
        MGDNet nk b v d -> MGD'Net b v d $ Identified s nk :| []
        MGDGenVar -> MGD'GenVar $ s :| []
        MGDTask a port param lp d b -> MGD'Task a s port param lp d b
        MGDFunc a t port param lp d b -> MGD'Func a t s port param lp d b
    )
    ( \(AttrIded na s mgd) (Attributed a mgd') ->
        if na /= a
          then Nothing
          else case (mgd', mgd) of
            (MGD'BlockDecl l, MGDBlockDecl bd) ->
              Just $ Attributed a $ MGD'BlockDecl $ Identified s bd <| l
            (MGD'Net b' v' d' l, MGDNet nk b v d)
              | b == b' && v == v' && d == d' ->
                Just $ Attributed a $ MGD'Net b' v' d' $ Identified s nk <| l
            (MGD'GenVar l, MGDGenVar) -> Just $ Attributed a $ MGD'GenVar $ s <| l
            _ -> Nothing
    )
  where
    mauto b = pift "automatic" b
    pPort (Attributed a (d, t, l)) =
      prettyAttr a
        <?=> nest
          ( group
              ( viaShow d <=> case t of
                  TFTRegSignRange r sr -> group (pift "reg" r <?=> prettySignRange sr)
                  TFTComType t -> viaShow t
              )
              <=> padj (cslid1 prettyIdent) l
          )

data ModGenItem'
  = MGI'ContAss DriveStrength (Maybe Delay3) (NonEmpty NetAssign)
  | MGI'GateInst (NonEmpty (Identified (Maybe Range2, GateInst)))
  | MGI'UDPInst
      B.ByteString
      DriveStrength
      (Maybe Delay2)
      (NonEmpty (Identified (Maybe Range2, NetLValue, NonEmpty Expr)))
  | MGI'ModInst B.ByteString ParamAssign (NonEmpty (Identified (Maybe Range2, PortAssign)))
  | MGI'UnknownInst
      B.ByteString
      (Maybe (Either Expr (Expr, Expr)))
      (NonEmpty (Identified (Maybe Range2, NetLValue, NonEmpty Expr)))
  | MGI'Initial AttrStmt
  | MGI'Always AttrStmt
  | MGI'LoopGen (Identified CExpr) CExpr (Identified CExpr) GenerateBlock
  | MGI'If CExpr (Maybe GenerateBlock) (Maybe GenerateBlock)
  | MGI'Case CExpr [GenCaseItem] (Maybe GenerateBlock)

mkmgi' :: ModGenItem -> ModGenItem'
mkmgi' mgi = case mgi of
  MGIContAss ds d3 ass -> MGI'ContAss ds d3 $ ass :| []
  MGIGateInst s rng gi -> MGI'GateInst $ Identified s (rng, gi) :| []
  MGIUDPInst kind ds d2 s rng lv args -> MGI'UDPInst kind ds d2 $ Identified s (rng, lv, args) :| []
  MGIModInst kind param s rng args -> MGI'ModInst kind param $ Identified s (rng, args) :| []
  MGIUnknownInst kind param s rng lv args ->
    MGI'UnknownInst kind param $ Identified s (rng, lv, args) :| []
  MGIInitial s -> MGI'Initial s
  MGIAlways s -> MGI'Always s
  MGILoopGen ini cond upd s -> MGI'LoopGen ini cond upd s
  MGIIf c t f -> MGI'If c t f
  MGICase c b d -> MGI'Case c b d

ppMGI' :: Attributed ModGenItem' -> Doc
ppMGI' (Attributed a x) = case x of
  MGI'ContAss ds d3 l ->
    prettyAttr a
      <?=> ng
        ( group
            ( "assign" <?=> prettyDriveStrength ds
                <?=> pm prettyDelay3 d3
            )
            <=> gpadj (cslid1 prettyNetAssign) l
        )
      <> semi
  MGI'GateInst l -> prettyGateInsts (Attributed a $ NE.reverse l)
  MGI'UDPInst kind ds d2 l ->
    prettyAttr a
      <?=> ng
        ( group (raw kind <?=> prettyDriveStrength ds <?=> pm prettyDelay2 d2)
            <=> csl1
              ( \(Identified s (rng, lv, args)) ->
                  ng $
                    pidr s rng
                      <> group
                        ( par $
                            gpadj prettyNetLV lv <.> padj (rcslid1 $ mkg prettyExpr) args
                        )
              )
              l
        )
      <> semi
  MGI'ModInst kind param l ->
    prettyAttr a
      <?=> ng
        ( group
            ( raw kind <?=> case param of
                ParamPositional l -> cslid ("#(" <> softspace) rparen (\s -> first ng . prettyExpr s) l
                ParamNamed l ->
                  csl
                    ("#(" <> softspace)
                    (softline <> rparen)
                    ( \(Identified i e) ->
                        ng $ dot <> padj prettyIdent i <> group (par $ pm (padj prettyMTM) e)
                    )
                    l
            )
            <=> csl1
              ( \(Identified s (rng, args)) ->
                  ng $
                    pidr s rng
                      <> par
                        ( case args of
                            PortPositional l ->
                              rcslid
                                mempty
                                mempty
                                ( \s (Attributed a e) ->
                                    first ((prettyAttr a <?=>) . ng) $ maybe (mempty, s) (prettyExpr s) e
                                )
                                l
                            PortNamed l ->
                              rcsl
                                mempty
                                softline
                                ( \(AttrIded a i e) ->
                                    group $
                                      prettyAttr a
                                        <?=> dot <> ng (padj prettyIdent i <> group (par $ pm (padj prettyExpr) e))
                                )
                                l
                        )
              )
              l
        )
      <> semi
  MGI'UnknownInst kind param l ->
    prettyAttr a
      <?=> ng
        ( group
            ( raw kind
                <?=> pm
                  ( ("#" <>)
                      . either
                        (\e -> par $ padj prettyExpr e)
                        (\(e0, e1) -> par $ gpadj prettyExpr e0 <.> gpadj prettyExpr e1)
                  )
                  param
            )
            <=> csl1
              ( \(Identified s (rng, lv, args)) ->
                  ng $
                    pidr s rng
                      <> group (par $ gpadj prettyNetLV lv <.> padj (rcslid1 $ mkg prettyExpr) args)
              )
              l
        )
      <> semi
  MGI'Initial s -> prettyAttr a <?=> ng ("initial" <=> prettyAttrStmt s)
  MGI'Always s -> prettyAttr a <?=> ng ("always" <=> prettyAttrStmt s)
  MGI'LoopGen (Identified si vi) cond (Identified su vu) s ->
    prettyAttr a
      <?=> ng
        ( group
            ( "for"
                <=> group
                  ( par $
                      let (di, li) = prettyCExpr softline vi
                          (du, lu) = prettyCExpr softline vu
                       in ng (raw si <=> equals <+> group di) <> li
                            <> semi <+> ngpadj prettyCExpr cond
                            <> semi <+> ng (raw su <=> equals <+> group du)
                            <> lu
                  )
            )
            <=> prettyGenerateBlock s
        )
  MGI'If c t f ->
    let head = "if" <=> group (par $ padj prettyCExpr c)
     in ( if genDanglingElse f t
            then block (prettyAttr a <?=> ng head <=> "begin") "end"
            else \x -> prettyAttr a <?=> ng (group head <=> x)
        )
          (maybe semi prettyGenerateBlock t)
          <?#> pm
            ( \f -> case f of
                GBSingle (GIMGI (Attributed _ (MGIIf _ _ _) :| [])) -> group $ "else" <=> prettyGenerateBlock f
                _ -> ng $ "else" <=> prettyGenerateBlock f
            )
            f
  MGI'Case c b md ->
    block
      (prettyAttr a <?=> nest ("case" <=> group (par $ padj prettyCExpr c)))
      "endcase"
      $ pf
        (flip (<#>))
        mempty
        mempty
        ( \(GenCaseItem p v) ->
            gpadj (cslid1 prettyCExpr) p <> colon <+> ng (maybe semi prettyGenerateBlock v)
        )
        b
        <?#> pm (\d -> nest $ "default:" <=> prettyGenerateBlock d) md
  where
    pidr i r = piff (gpadj (pidWith prettyIdent softline $ pm prettyRange2 r) i) $ B.null i

prettyModGenItems :: Foldable f => f (Attributed ModGenItem) -> Doc
prettyModGenItems =
  prettyregroup
    ppMGI'
    (\(Attributed a mgi) -> Attributed a $ mkmgi' mgi)
    ( \(Attributed na mgi) (Attributed a mgi') ->
        if na /= a
          then Nothing
          else case (mgi', mgi) of
            (MGI'ContAss ds' d3' l, MGIContAss ds d3 ass)
              | ds == ds' && d3 == d3' ->
                Just $ Attributed a $ MGI'ContAss ds' d3' $ ass <| l
            (MGI'GateInst l, MGIGateInst s rng gi) ->
              Just $ Attributed a $ MGI'GateInst $ Identified s (rng, gi) <| l
            (MGI'UDPInst k' ds' d2' l, MGIUDPInst k ds d2 s rng lv arg)
              | k == k' && ds == ds' && d2 == d2' ->
                Just $ Attributed a $ MGI'UDPInst k' ds' d2' $ Identified s (rng, lv, arg) <| l
            (MGI'ModInst k' p' l, MGIModInst k p s rng arg)
              | k == k' && p == p' ->
                Just $ Attributed a $ MGI'ModInst k' p' $ Identified s (rng, arg) <| l
            (MGI'UnknownInst k' p' l, MGIUnknownInst k p s rng lv arg)
              | k == k' && p == p' ->
                Just $ Attributed a $ MGI'UnknownInst k' p' $ Identified s (rng, lv, arg) <| l
            _ -> Nothing
    )

prettyParamOvers :: Foldable f => f ParamOver -> Doc
prettyParamOvers =
  prettyregroup
    ( \(Attributed a l) ->
        prettyAttr a
          <?=> ng
            ( "defparam"
                <=> padj
                  ( cslid1 $ \s (hi, v) ->
                      first (\x -> group $ psp prettyHierIdent hi <> equals <+> x) $ prettyCMTM s v
                  )
                  l
                  <> semi
            )
    )
    (\(ParamOver a hi v) -> Attributed a $ (hi, v) :| [])
    ( \(ParamOver na hi v) (Attributed a l) ->
        if na == a then Just $ Attributed a $ (hi, v) <| l else Nothing
    )

prettySpecParamDecls :: Maybe Range2 -> NonEmpty SpecParamDecl -> Doc
prettySpecParamDecls rng l =
  nest $
    group ("specparam" <?=> pm prettyRange2 rng)
      <=> gpadj
        ( cslid1 $ \s d -> first ng $ case d of
            SPDPathPulse i@(Identified j m) o@(Identified k _) rej err ->
              ( group
                  ( "PATHPULSE$" <> ngpadj prettySpecTerm i
                      <> piff "$" ((m == Nothing && maybe True ((c2w '\\' /=) . fst) (B.uncons j)) || B.null k)
                      <> ng (fst $ prettySpecTerm mempty o)
                  )
                  <=> equals
                    <+> group
                      ( par $
                          ngpadj prettyCMTM rej
                            <> piff (comma <+> ngpadj prettyCMTM err) (rej == err)
                      ),
                s
              )
            SPDAssign (Identified i v) ->
              first (\x -> raw i <=> equals <+> ng x) $ prettyCMTM s v
        )
        l
        <> semi
  where
    pnid f x@(Identified s _) = piff (f x) (B.null s)

data ModuleItem'
  = MI'MGI (NonEmpty (Attributed ModGenItem))
  | MI'GenReg GenerateRegion
  | MI'SpecBlock [SpecParam] [SpecifyItem]

prettyModuleItems :: [ModuleItem] -> Doc
prettyModuleItems =
  prettyregroup
    ( \x -> case x of
        MI'MGI mgi -> prettyModGenItems $ NE.reverse mgi
        MI'GenReg gr -> block "generate" "endgenerate" $ prettyGenerateRegion gr
        MI'SpecBlock sp si ->
          block "specify" "endspecify" $
            prettyregroup
              (uncurry prettySpecParamDecls)
              (\(SpecParam rng d) -> (rng, d :| []))
              (\(SpecParam nrng d) (rng, l) -> if nrng == rng then Just $ (rng, d <| l) else Nothing)
              sp
              <?#> prettySpecifyItems si
    )
    ( \mi -> case mi of
        MIMGI mgi -> MI'MGI $ mgi :| []
        MIGenReg gr -> MI'GenReg gr
        MISpecBlock sp si -> MI'SpecBlock sp si
    )
    ( \mi mi' -> case (mi', mi) of
        (MI'MGI l, MIMGI mgi) -> Just $ MI'MGI $ mgi <| l
        _ -> Nothing
    )

data PortDecl'
  = PD'In (Maybe NetType) SignRange (NonEmpty B.ByteString)
  | PD'InOut (Maybe NetType) SignRange (NonEmpty B.ByteString)
  | PD'Out (Maybe NetType) SignRange (NonEmpty B.ByteString)
  | PD'OutReg SignRange (NonEmpty (Identified (Maybe CExpr)))
  | PD'OutVar Bool (NonEmpty (Identified (Maybe CExpr)))

prettyPortDecls :: [AttrIded PortDecl] -> Doc
prettyPortDecls =
  prettyregroup
    ( \(Attributed a x) ->
        prettyAttr a
          <?=> ng
            ( ( case x of
                  PD'In nt sr l ->
                    group ("input" <?=> pnt nt <?=> prettySignRange sr) <=> gpadj (cslid1 prettyIdent) l
                  PD'InOut nt sr l ->
                    group ("inout" <?=> pnt nt <?=> prettySignRange sr) <=> gpadj (cslid1 prettyIdent) l
                  PD'Out nt sr l ->
                    group ("output" <?=> pnt nt <?=> prettySignRange sr) <=> gpadj (cslid1 prettyIdent) l
                  PD'OutReg sr l -> group ("output" <=> "reg" <?=> prettySignRange sr) <=> gpadj (cslid1 pmv) l
                  PD'OutVar it l ->
                    group ("output" <=> if it then "integer" else "time") <=> gpadj (cslid1 pmv) l
              )
            )
          <> semi
    )
    ( \(AttrIded a s pd) -> Attributed a $ case pd of
        PDIn nt sr -> PD'In nt sr $ s :| []
        PDInOut nt sr -> PD'InOut nt sr $ s :| []
        PDOut nt sr -> PD'Out nt sr $ s :| []
        PDOutReg sr e -> PD'OutReg sr $ Identified s e :| []
        PDOutVar it e -> PD'OutVar it $ Identified s e :| []
    )
    ( \(AttrIded na s pd) (Attributed a pd') ->
        if na /= a
          then Nothing
          else case (pd', pd) of
            (PD'In nt' sr' l, PDIn nt sr)
              | nt == nt' && sr == sr' ->
                Just $ Attributed a $ PD'In nt' sr' $ s <| l
            (PD'InOut nt' sr' l, PDInOut nt sr)
              | nt == nt' && sr == sr' ->
                Just $ Attributed a $ PD'InOut nt' sr' $ s <| l
            (PD'Out nt' sr' l, PDOut nt sr)
              | nt == nt' && sr == sr' ->
                Just $ Attributed a $ PD'Out nt' sr' $ s <| l
            (PD'OutReg sr' l, PDOutReg sr v)
              | sr == sr' ->
                Just $ Attributed a $ PD'OutReg sr' $ Identified s v <| l
            (PD'OutVar it' l, PDOutVar it v)
              | it == it' ->
                Just $ Attributed a $ PD'OutVar it' $ Identified s v <| l
            _ -> Nothing
    )
  where
    pnt = maybe mempty viaShow
    pmv s (Identified i mv) =
      maybe (prettyIdent s i) (\v -> first (\x -> ng $ raw i <=> equals <+> x) $ prettyCExpr s v) mv

prettyGenerateRegion :: GenerateRegion -> Doc
prettyGenerateRegion (GenerateRegion lp po d b) =
  prettyXparam "localparam" lp
    <?#> prettyParamOvers po
    <?#> prettyModGenDecls d
    <?#> prettyModGenItems b

prettyGenerateBlock :: GenerateBlock -> Doc
prettyGenerateBlock x = case x of
  GBSingle i -> case i of -- assumes items can be regrouped
    GIParam lp -> prettyXparam "localparam" $ NE.toList lp
    GIParamOver l -> prettyParamOvers $ NE.toList l
    GIMGD l -> prettyModGenDecls $ NE.toList l
    GIMGI l -> prettyModGenItems $ NE.toList l
  GBBlock (Identified s r) ->
    block ("begin" <> piff (colon <=> raw s) (B.null s)) "end" $ prettyGenerateRegion r

prettyModuleBlock :: LocalCompDir -> ModuleBlock -> (Doc, LocalCompDir)
prettyModuleBlock (LocalCompDir ts c p dn) (ModuleBlock a i pi pd params lp d sp b mts mc mp mdn) =
  ( piff (let (a, b) = fromJust mts in "`timescale" <+> tsval a <+> "/" <+> tsval b) (ts == mts)
      <?#> piff (if mc then "`celldefine" else "`endcelldefine") (c == mc)
      <?#> piff
        ( maybe
            "`nounconnected_drive"
            (\b -> "`unconnected_drive pull" <> if b then "1" else "0")
            mp
        )
        (p == mp)
      <?#> piff ("`default_nettype" <+> maybe "none" viaShow mdn) (dn == mdn)
      <?#> block
        ( let (d, s) = prettyIdent softline i
           in prettyAttr a
                <?=> ng
                  ( group ("module" <=> d)
                      <> s
                      <> group
                        ( par $
                            rcslid
                              mempty
                              mempty
                              ( \s (Identified i l) -> case l of
                                  [p@(Identified i' _)] | i == i' -> mkg prettySpecTerm s p
                                  _ ->
                                    if B.null i
                                      then portexpr s l
                                      else (dot <> padj prettyIdent i <> par (padj portexpr l), s)
                              )
                              pi
                        )
                  )
                <> semi
        )
        "endmodule"
        ( prettyXparam "parameter" params <?#> prettyXparam "localparam" lp
            <?#> prettyregroup
              (\(Attributed a (rng, l)) -> prettyAttr a <?=> ng (prettySpecParamDecls rng (NE.reverse l)))
              (\(Attributed a (SpecParam rng d)) -> Attributed a (rng, d :| []))
              ( \(Attributed na (SpecParam nrng d)) (Attributed a (rng, l)) ->
                  if na == a && nrng == rng then Just $ Attributed a (rng, d <| l) else Nothing
              )
              sp
            <?#> prettyPortDecls pd
            <?#> prettyModGenDecls d
            <?#> prettyModuleItems b
        ),
    LocalCompDir mts mc mp mdn
  )
  where
    portexpr s l = case l of
      [] -> (mempty, mempty)
      [p] -> prettySpecTerm s p
      _ -> (cslid (lbrace <> softspace) rbrace prettySpecTerm l, s)
    tsval i =
      let (u, v) = divMod i 3
       in case v of 0 -> "1"; 1 -> "10"; 2 -> "100"
            <> case u of 0 -> "s"; -1 -> "ms"; -2 -> "us"; -3 -> "ns"; -4 -> "ps"; -5 -> "fs"

prettySeqRows :: NonEmpty SeqRow -> Doc
prettySeqRows l@(h :| t) =
  pf
    (<#>)
    mempty
    mempty
    ( maybe
        -- fallback prettyprinting because aligned is better but not always possible
        (\(SeqRow si s ns) -> nest $ viaShow si <=> prettyend s ns)
        -- aligned prettyprinting
        (\ll (SeqRow si s ns) -> nest $ prettystart ll si <=> prettyend s ns)
        $ foldl
          ( \msl sr ->
              msl >>= \sl ->
                if totallength sr /= tl
                  then Nothing
                  else case _SRowInput sr of
                    SISeq l0 e l1 ->
                      let w = edgeprintsize e
                          spliceputat n l =
                            nonEmpty
                              Nothing
                              ( \(h :| t) ->
                                  either
                                    ( \m ->
                                        if m <= n
                                          then (Left m :) <$> spliceputat (n - m) t
                                          else Just $ Left n : Right w : Left (m - n - 1) : t
                                    )
                                    ( \w' ->
                                        if 0 < n
                                          then (Right w' :) <$> spliceputat (n - 1) t
                                          else Just $ Right (max w w') : t
                                    )
                                    h
                              )
                              l
                       in spliceputat (length l0) sl
                    SIComb l -> Just sl
          )
          ( Just $ case _SRowInput h of
              SISeq l0 e l1 -> [Left $ length l0, Right $ edgeprintsize e, Left $ length l1]
              SIComb l -> [Left $ length l]
          )
          t
    )
    l
  where
    tl = totallength h
    totallength (SeqRow si _ _) =
      case si of SIComb l -> length l; SISeq l0 e l1 -> 1 + length l0 + length l1
    edgeprintsize x = case x of
      EdgePos_neg _ -> 1
      EdgeDesc _ _ -> 4
    sp3 = raw "   "
    prettyend s ns = group (colon <+> viaShow s <=> colon <+> maybe "-" viaShow ns) <> semi
    prettylevelwithwidth l w = viaShow l <> pift sp3 (w == 4)
    concat = pf (<>) mempty mempty
    prettystart ll si =
      snd $
        foldl
          ( \(si, d) e -> case (e, si) of
              (Left n, Left l') ->
                let (l0, l1) = splitAt n l'
                 in (Left l1, d <> concat viaShow l0)
              (Right w, Left (h : t)) -> (Left t, d <> prettylevelwithwidth h w)
              (Left n, Right (l0, e, l1)) ->
                let (l00, l01) = splitAt n l0
                 in (Right (l01, e, l1), d <> concat viaShow l00)
              (Right w, Right ([], e, l1)) -> (Left l1, d <> viaShow e <> pift sp3 (edgeprintsize e < w))
              (Right w, Right (h : t, e, l')) -> (Right (t, e, l'), d <> prettylevelwithwidth h w)
          )
          (case si of SIComb l -> Left $ NE.toList l; SISeq a b c -> Right (a, b, c), mempty)
          ll

prettyPrimitiveBlock :: PrimitiveBlock -> Doc
prettyPrimitiveBlock (PrimitiveBlock a s (oa, oi) i b) =
  block
    ( prettyAttr a
        <?=> ng
          ( group ("primitive" <=> od) <> os
              <> group
                ( par $
                    od <> os <.> padj (cslid1 $ \s -> prettyIdent s . snd) i
                )
          )
        <> semi
    )
    "endprimitive"
    $ ( prettyAttr oa
          <?=> ( case b of
                   SeqTable (Left e) _ _ ->
                     ng $ group ("output" <=> "reg" <=> od) <=> equals <+> gpadj prettyCExpr e
                   SeqTable (Right a') _ _ | a' == oa -> ng $ "output reg" <=> od <> os
                   SeqTable (Right a') _ _
                     | a' /= oa ->
                       ng ("output" <=> od <> os) <> semi
                         <#> prettyAttr a' <?=> ng ("reg" <=> od <> os)
                   _ -> ng $ "output" <=> od <> os
               )
          <> semi
      )
      <#> prettyregroup
        (\(a, l) -> prettyAttr a <?=> ng ("input" <=> gpadj (cslid1 prettyIdent) l) <> semi)
        (\(a, i) -> (a, i :| []))
        (\(na, i) (a, l) -> if na /= a then Nothing else Just (a, i <| l))
        i
      <#> case b of
        CombTable l ->
          block "table" "endtable" $
            pf
              (<#>)
              mempty
              mempty
              (\(CombRow i o) -> nest $ fromString (concatMap show i) <=> colon <+> viaShow o <> semi)
              l
        SeqTable _ mi l ->
          pm
            ( \iv ->
                nest $
                  group ("initial" <=> od)
                    <=> equals <+> case iv of { ZOXX -> "1'bx"; ZOXZ -> "0"; ZOXO -> "1" } <> semi
            )
            mi
            <?#> block "table" "endtable" (prettySeqRows l)
  where
    (od, os) = prettyIdent softline oi
    (d, l) = prettyIdent softline s

prettyVerilog2005 :: Verilog2005 -> Doc
prettyVerilog2005 (Verilog2005 mb po pb cb) =
  fst (foldr (\m (d, lcd) -> first (d <##>) $ prettyModuleBlock lcd m) (mempty, lcdDefault) mb)
    <##> pl prettyPrimitiveBlock pb
    <##> pl
      ( \(ConfigBlock i des b def) ->
          block
            (nest $ "config" <=> padj prettyIdent i <> semi)
            "endconfig"
            $ nest ("design" <?=> catid prettyDot1Ident des <> semi)
              <#> pf
                (flip (<#>))
                mempty
                mempty
                ( \(ConfigItem ci llu) ->
                    nest $
                      ng
                        ( case ci of
                            Right i -> "instance" <=> foldrMap1 (padj prettyIdent) (\a b -> padj prettyIdent a <> dot <> b) i
                            Left c -> "cell" <=> fst (prettyDot1Ident mempty c)
                        )
                        <=> ng
                          ( case llu of
                              LLUUse s c -> "use" <=> padj prettyDot1Ident s <> pift ":config" c
                              LLULiblist ls -> "liblist" <?=> catid prettyIdent ls
                          )
                        <> semi
                )
                b
              <#> nest ("default liblist" <?=> catid prettyIdent def) <> semi
      )
      cb
    <##> nonEmpty
      mempty
      (block "module ___all_parameter_override___;" "endmodule" . prettyParamOvers)
      po
  where
    catid :: PrettyIdent a -> [a] -> Doc
    catid f =
      nonEmpty mempty $
        foldrMap1 (gpadj f) $ (<>) . gpsp f
    (<##>) = mkopt $ \a b -> a <#> mempty <#> b
    pl :: (a -> Doc) -> [a] -> Doc
    pl = pf (flip (<##>)) mempty mempty
