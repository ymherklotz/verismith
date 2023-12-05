-- Module      : Verismith.Verilog2005.Generator
-- Description : AST random generator
-- Copyright   : (c) 2023 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : experimental
-- Portability : POSIX
{-# LANGUAGE OverloadedLists #-}

module Verismith.Verilog2005.Generator
  ( runGarbageGeneration,
    GeneratorOpts,
    GenM,
    defGeneratorOpts,
  )
where

import Control.Applicative (liftA2, liftA3)
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Lazy
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.IntMap.Strict as IntMap
import Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.List.NonEmpty as NE
import Data.Tuple
import qualified Data.Vector.Unboxed as VU
import Numeric.Natural
import System.Random.MWC.Probability
import Verismith.Config
import Verismith.Utils (liftA4, liftA5, mkpair)
import Verismith.Verilog2005.AST
import Verismith.Verilog2005.Lexer
import Verismith.Verilog2005.Randomness

infixl 4 <.>

(<.>) :: (Monad m, Applicative m) => m (a -> m b) -> m a -> m b
(<.>) mf mx = join $ mf <*> mx

attenuateCat :: (NonEmpty (Double, a)) -> Double -> CategoricalProbability -> CategoricalProbability
attenuateCat l d p = case p of
  CPDiscrete wl -> CPDiscrete $ NE.zipWith (\a w -> w * d ** (fst a)) l wl
  CPBiasedUniform wl wb ->
    let im = IntMap.fromListWith (+) $ map swap wl
     in CPDiscrete $
          NE.map (\(k, a) -> IntMap.findWithDefault wb k im * d ** (fst a)) $
            NE.zip [0 ..] l

attenuateNum :: Double -> NumberProbability -> NumberProbability
attenuateNum d p =
  if d == 1
    then p
    else case p of
      NPUniform l h ->
        NPDiscrete $
          if d == 0 then [(1, l)] else NE.fromList (zipWith mkdistrfor [1 ..] [l .. h])
      NPBinomial off t p ->
        NPDiscrete $
          if d == 0
            then [(1, off)]
            else error "Recursive attenuation does not support binomial distribution"
      NPNegativeBinomial off pf f ->
        if d == 0
          then NPDiscrete [(1, off)]
          else NPNegativeBinomial off pf f
      NPPoisson off p ->
        if d == 0
          then NPDiscrete [(1, off)]
          else NPPoisson off (p * d ** (fromInteger $ toInteger off))
      NPDiscrete l -> NPDiscrete $ if d == 0 then [NE.head l] else NE.map (uncurry mkdistrfor) l
      NPLinearComb l -> NPLinearComb $ NE.map (\(p, np) -> (p, attenuateNum d np)) l
  where
    mkdistrfor bw n = (bw * d ** (fromInteger $ toInteger n), n)

type GenM' = GenM GeneratorOpts

tameExprRecursion :: GenM' a -> GenM' a
tameExprRecursion =
  local $
    _1 %~ \go -> go {_goExprCurAttenuation = _goExprRecAttenuation go * _goExprCurAttenuation go}

tameAttrRecursion :: GenM' a -> GenM' a
tameAttrRecursion =
  local $
    _1 %~ \go -> go {_goAttribCurAttenuation = _goAttribRecAttenuation go * _goAttribCurAttenuation go}

tameStmtRecursion :: GenM' a -> GenM' a
tameStmtRecursion =
  local $
    _1 %~ \go -> go {_goStmtCurAttenuation = _goStmtRecAttenuation go * _goStmtCurAttenuation go}

sampleAttenuatedBranch ::
  (GeneratorOpts -> Double) -> (GeneratorOpts -> CategoricalProbability) -> (NonEmpty (Double, GenM' a)) -> GenM' a
sampleAttenuatedBranch f p l = do
  gen <- asks snd
  d <- asks $ p . fst
  a <- asks $ f . fst
  join $ sampleIn (toList $ NE.map snd l) gen (attenuateCat l a d)

idSimpleLetter :: B.ByteString -- 0-9$ are forbidden as first letters
idSimpleLetter = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789$"

digitCharacter :: B.ByteString
digitCharacter = "0123456789"

-- Start of actual generation

garbageIdent :: GenM' B.ByteString
garbageIdent =
  choice
    _goEscaped_Simple
    ( B.pack . (c2w '\\' :)
        <$> sampleN
          _goEscapedLetters
          (toEnum <$> sampleSegment _goEscapedLetter 33 127)
    )
    ( do
        fl <- sampleFromString _goSimpleLetter (B.take 53 idSimpleLetter)
        s <- sampleString _goSimpleLetters _goSimpleLetter idSimpleLetter
        let loop t =
              let s = B.cons fl t
               in if isKW s
                    then flip B.cons t <$> sampleFromString _goSimpleLetter idSimpleLetter >>= loop
                    else return s
        loop s
    )

garbageIdentified :: GenM' x -> GenM' (Identified x)
garbageIdentified = liftA2 Identified garbageIdent

garbageSysIdent :: GenM' B.ByteString
garbageSysIdent = sampleNEString _goSystemLetters _goSimpleLetter idSimpleLetter

garbageHierIdent :: GenM' HierIdent
garbageHierIdent =
  HierIdent
    <$> sampleN _goPaths (garbageIdentified $ sampleMaybe _goOptionalElement garbageCExpr)
    <*> garbageIdent

garbageInteger :: GenM' Natural
garbageInteger = parseDecimal <$> sampleString _goDecimalSymbols _goDecimalSymbol digitCharacter

garbageReal :: GenM' B.ByteString
garbageReal =
  choice
    _goFixed_Floating
    ( do
        p <- number
        f <- number
        return $ p <> "." <> f
    )
    ( do
        p <- number
        f <- sampleString _goDecimalSymbols _goDecimalSymbol digitCharacter
        s <- sampleFrom _goExponentSign ["", "+", "-"]
        e <- number
        return $ p <> (if B.null f then "" else B.cons (c2w '.') f) <> "e" <> s <> e
    )
  where
    number = sampleNEString _goDecimalSymbols _goDecimalSymbol digitCharacter

garbageNumIdent :: GenM' NumIdent
garbageNumIdent =
  sampleBranch
    _goIntRealIdent
    [ NINumber <$> garbageInteger,
      NIReal <$> garbageReal,
      NIIdent <$> garbageIdent
    ]

garbagePrim :: GenM' i -> GenM' r -> GenM' (GenPrim i r)
garbagePrim ident grng =
  sampleAttenuatedBranch
    _goExprCurAttenuation
    _goPrimary
    [ mknum $ NXZ <$> sampleBernoulli _goX_Z,
      mknum $ NBinary <$> sampleNE _goBinarySymbols (sampleEnum _goBinarySymbol),
      mknum $ NOctal <$> sampleNE _goOctalSymbols (sampleEnum _goOctalSymbol),
      mknum $ NDecimal <$> garbageInteger,
      mknum $ NHex <$> sampleNE _goHexadecimalSymbols (sampleEnum _goHexadecimalSymbol),
      (0, PrimReal <$> garbageReal),
      ( 0,
        ( \s ->
            PrimString $
              B.pack $
                map c2w $
                  "\"" ++ concatMap (\x -> case x of '"' -> "\\\""; '\\' -> "\\\\"; '\n' -> "\\n"; _ -> [x]) s ++ "\""
        )
          . map w2c
          <$> sampleN _goStringCharacters (sampleEnum _goStringCharacter)
      ),
      (0, liftA2 PrimIdent ident $ tameExprRecursion grng),
      recurse $ PrimConcat <$> sampleNE _goConcatenations gexpr,
      recurse $ liftA2 PrimMultConcat garbageCExpr $ sampleNE _goConcatenations gexpr,
      recurse $ liftA3 PrimFun ident garbageAttributes $ toList <$> sampleNE _goArguments gexpr,
      recurse $ liftA2 PrimSysFun garbageSysIdent $ sampleN _goArguments gexpr,
      recurse $ PrimMinTypMax <$> garbageGenMinTypMax gexpr
    ]
  where
    mknum x =
      ( 0,
        do
          sz <- sampleSegment _goLiteralWidth 0 65535
          liftA2
            (PrimNumber $ if sz == 0 then Nothing else Just $ toEnum sz)
            (sampleBernoulli _goSignedness)
            x
      )
    gexpr = garbageGenExpr ident grng
    recurse x = (1, tameExprRecursion x)

garbageGenExpr :: GenM' i -> GenM' r -> GenM' (GenExpr i r)
garbageGenExpr ident grng =
  sampleAttenuatedBranch
    _goExprCurAttenuation
    _goExpression
    [ (0, ExprPrim <$> garbagePrim ident grng),
      ( 0.5,
        tameExprRecursion $
          liftA3
            ExprUnOp
            (sampleEnum _goUnaryOperation)
            garbageAttributes
            $ garbagePrim ident grng
      ),
      ( 1,
        tameExprRecursion $
          liftA4
            ExprBinOp
            gexpr
            (sampleEnum _goBinaryOperation)
            garbageAttributes
            gexpr
      ),
      (2, tameExprRecursion $ liftA4 ExprCond gexpr garbageAttributes gexpr gexpr)
    ]
  where
    gexpr = garbageGenExpr ident grng

garbageGenMinTypMax :: GenM' e -> GenM' (GenMinTypMax e)
garbageGenMinTypMax gexpr =
  choice _goMinTypMax (liftA3 MTMFull gexpr gexpr gexpr) (MTMSingle <$> gexpr)

garbageRange2 :: GenM' Range2
garbageRange2 = liftA2 Range2 garbageCExpr garbageCExpr

garbageDims :: GenM' [Range2]
garbageDims = sampleN _goDimensions garbageRange2

garbageGenRangeExpr :: GenM' e -> GenM' (GenRangeExpr e)
garbageGenRangeExpr ge =
  sampleBranch
    _goRangeExpr
    [ GRESingle <$> ge,
      GREPair <$> garbageRange2,
      liftA3 GREBaseOff ge (sampleBernoulli _goRangeOffsetPos_Neg) garbageCExpr
    ]

garbageGenDimRange :: GenM' e -> GenM' (GenDimRange e)
garbageGenDimRange ge = liftA2 GenDimRange (sampleN _goDimensions ge) (garbageGenRangeExpr ge)

garbageExpr :: GenM' Expr
garbageExpr =
  Expr
    <$> garbageGenExpr
      (tameExprRecursion garbageHierIdent)
      (sampleMaybe _goOptionalElement garbageDimRange)

garbageCExpr :: GenM' CExpr
garbageCExpr =
  CExpr <$> garbageGenExpr garbageIdent (sampleMaybe _goOptionalElement garbageCRangeExpr)

garbageRangeExpr :: GenM' RangeExpr
garbageRangeExpr = garbageGenRangeExpr garbageExpr

garbageCRangeExpr :: GenM' CRangeExpr
garbageCRangeExpr = garbageGenRangeExpr garbageCExpr

garbageDimRange :: GenM' DimRange
garbageDimRange = DimRange <$> garbageGenDimRange garbageExpr

garbageCDimRange :: GenM' CDimRange
garbageCDimRange = CDimRange <$> garbageGenDimRange garbageCExpr

garbageMinTypMax :: GenM' MinTypMax
garbageMinTypMax = garbageGenMinTypMax garbageExpr

garbageCMinTypMax :: GenM' CMinTypMax
garbageCMinTypMax = garbageGenMinTypMax garbageCExpr

garbageAttributes :: GenM' [Attribute]
garbageAttributes = do
  gen <- asks snd
  attrn <- asks $ _goAttributes . fst
  att <- asks $ _goAttribCurAttenuation . fst
  n <- sampleNumberProbability gen $ attenuateNum att attrn
  sequence $
    replicate n $
      garbageIdentified $
        sampleMaybe _goOptionalElement $
          tameAttrRecursion garbageCExpr

garbageAttributed :: GenM' x -> GenM' (Attributed x)
garbageAttributed = liftA2 Attributed garbageAttributes

garbageAttrIded :: GenM' x -> GenM' (AttrIded x)
garbageAttrIded = liftA3 AttrIded garbageAttributes garbageIdent

garbageDelay1 :: GenM' Delay1
garbageDelay1 =
  sampleBranch
    _goDelay
    [ D1Base <$> garbageNumIdent,
      D11 <$> garbageMinTypMax
    ]

garbageDelay2 :: GenM' Delay2
garbageDelay2 =
  sampleBranch
    _goDelay
    [ D2Base <$> garbageNumIdent,
      D21 <$> garbageMinTypMax,
      liftA2 D22 garbageMinTypMax garbageMinTypMax
    ]

garbageDelay3 :: GenM' Delay3
garbageDelay3 =
  sampleBranch
    _goDelay
    [ D3Base <$> garbageNumIdent,
      D31 <$> garbageMinTypMax,
      liftA2 D32 garbageMinTypMax garbageMinTypMax,
      liftA3 D33 garbageMinTypMax garbageMinTypMax garbageMinTypMax
    ]

garbageLValue :: GenM' dr -> GenM' (LValue dr)
garbageLValue gdr =
  sampleN _goLValues (garbageLValue gdr) >>= \l -> case l of
    [] -> liftA2 LVSingle garbageHierIdent $ sampleMaybe _goOptionalElement gdr
    h : t -> return $ LVConcat $ h :| t

garbageNetLV :: GenM' NetLValue
garbageNetLV = garbageLValue garbageCDimRange

garbageVarLV :: GenM' VarLValue
garbageVarLV = garbageLValue garbageDimRange

garbageVarAssign :: GenM' VarAssign
garbageVarAssign = liftA2 VarAssign garbageVarLV garbageExpr

garbageNetAssign :: GenM' NetAssign
garbageNetAssign = liftA2 NetAssign garbageNetLV garbageExpr

garbageStatement :: GenM' Statement
garbageStatement =
  sampleAttenuatedBranch
    _goStmtCurAttenuation
    _goStatement
    [ ( 0.5,
        liftA2
          SProcTimingControl
          (sampleBranch _goDelayEvent [Left <$> garbageDelay1, Right <$> evctl])
          $ tameStmtRecursion garbageMybStmt
      ),
      ( 2,
        liftA3
          SBlock
          ( sampleMaybe
              _goOptionalElement
              (liftA2 (\s (p, lp, d) -> (s, p, lp, d)) garbageIdent garbageStdBlockDecl)
          )
          (sampleBernoulli _goBlockPar_Seq)
          (sampleN _goOtherItems garbageAttrStmt)
      ),
      ( 0,
        liftA3
          SBlockAssign
          (sampleBernoulli _goAssignmentBlocking)
          garbageVarAssign
          (sampleMaybe _goOptionalElement delevctl)
      ),
      ( 2,
        do
          d <- tameStmtRecursion garbageMybStmt
          n <- (if d == Attributed [] Nothing then succ else id) <$> sampleNum _goCaseBranches
          liftA3 (\a b c -> SCase a b c d) (sampleEnum _goCaseStatement) gexpr $
            sequence $
              replicate n $
                liftA2 CaseItem (sampleNE _goArguments gexpr) (tameStmtRecursion garbageMybStmt)
      ),
      (1, liftA3 SIf gexpr (tameStmtRecursion garbageMybStmt) (tameStmtRecursion garbageMybStmt)),
      ( 0.5,
        liftA2
          SLoop
          ( sampleBranch
              _goLoopStatement
              [ pure LSForever,
                LSRepeat <$> gexpr,
                LSWhile <$> gexpr,
                liftA3 LSFor garbageVarAssign gexpr garbageVarAssign
              ]
          )
          (tameStmtRecursion garbageAttrStmt)
      ),
      (0.5, liftA2 SWait gexpr $ tameStmtRecursion garbageMybStmt),
      (0, hie STaskEnable),
      ( 0,
        liftA2 SSysTaskEnable garbageSysIdent $
          sampleN _goArguments $ sampleMaybe _goOptionalElement gexpr
      ),
      ( 0,
        SProcContAssign
          <$> sampleBranch
            _goProcContAssign
            [ PCAAssign <$> garbageVarAssign,
              PCADeassign <$> garbageVarLV,
              PCAForce <$> sampleEither _goPCAVar_Net garbageVarAssign garbageNetAssign,
              PCARelease <$> sampleEither _goPCAVar_Net garbageVarLV garbageNetLV
            ]
      ),
      (0, SDisable <$> garbageHierIdent),
      (0, hie SEventTrigger)
    ]
  where
    gexpr = garbageExpr
    hie f = liftA2 f garbageHierIdent $ sampleN _goArguments garbageExpr
    evprm = liftA2 EventPrim (sampleEnum _goEventPrefix) gexpr
    evctl =
      sampleBranch
        _goEvent
        [ pure ECDeps,
          ECIdent <$> garbageHierIdent,
          ECExpr <$> sampleNE _goEvents evprm
        ]
    delevctl =
      sampleBranch
        _goDelayEvent
        [ DECDelay <$> garbageDelay1,
          DECEvent <$> evctl,
          liftA2 DECRepeat garbageExpr evctl
        ]

garbageMybStmt :: GenM' MybStmt
garbageMybStmt = garbageAttributed $ sampleMaybe _goOptionalElement garbageStatement

garbageAttrStmt :: GenM' AttrStmt
garbageAttrStmt = garbageAttributed garbageStatement

garbageSR :: GenM' SignRange
garbageSR =
  liftA2 SignRange (sampleBernoulli _goSignedness) (sampleMaybe _goOptionalElement garbageRange2)

garbageFPT :: GenM' FunParType
garbageFPT =
  choice
    _goTypeAbstract_Concrete
    (FPTComType <$> sampleEnum _goAbstractType)
    (FPTSignRange <$> garbageSR)

garbageParameter :: GenM' Parameter
garbageParameter = liftA4 Parameter garbageAttributes garbageIdent garbageFPT garbageCMinTypMax

garbageParamOver :: GenM' ParamOver
garbageParamOver = liftA3 ParamOver garbageAttributes garbageHierIdent garbageCMinTypMax

garbageBlockDecl :: GenM' t -> GenM' (BlockDecl t)
garbageBlockDecl m =
  sampleBranch
    _goBlockDeclType
    [ BDInt <$> m,
      BDTime <$> m,
      BDReal <$> m,
      BDRealTime <$> m,
      liftA2 BDReg garbageSR m,
      BDEvent <$> garbageDims
    ]

garbageStdBlockDecl :: GenM' ([Parameter], [Parameter], StdBlockDecl)
garbageStdBlockDecl =
  liftA3
    (,,)
    (sampleN _goParameters garbageParameter)
    (sampleN _goLocalParameters garbageParameter)
    $ sampleN _goDeclarations $ garbageAttrIded $ garbageBlockDecl garbageDims

garbageDriveStrength :: GenM' DriveStrength
garbageDriveStrength = do
  x <- strall
  y <- strall
  case (x, y) of
    (Just a, Just b) -> return $ DSNormal a b
    (Nothing, Just b) -> return $ DSHighZ False b
    (Just a, Nothing) -> return $ DSHighZ True a
    _ -> garbageDriveStrength
  where
    strall = sampleMaybeEnum _goDriveStrength

garbageNetKind :: GenM' NetKind
garbageNetKind =
  choice
    _goNet_Tri
    ( liftA2 NKNet (sampleEnum _goNetType) $
        sampleEither _goDimension_Initialisation garbageDims $
          mkpair garbageDriveStrength garbageExpr
    )
    ( choice
        _goDimension_Initialisation
        (liftA2 NKTriC (sampleEnum _goChargeStrength) garbageDims)
        (liftA2 NKTriD garbageDriveStrength garbageExpr)
    )

garbageTFT :: GenM' TaskFunType
garbageTFT =
  choice
    _goTypeAbstract_Concrete
    (TFTComType <$> sampleEnum _goAbstractType)
    (liftA2 TFTRegSignRange (sampleBernoulli _goRegister) garbageSR)

garbageModGenDecl :: GenM' (AttrIded ModGenDecl)
garbageModGenDecl =
  garbageAttrIded $
    sampleBranch
      _goModGenDeclaration
      [ MGDBlockDecl
          <$> garbageBlockDecl (sampleEither _goDimension_Initialisation garbageDims garbageCExpr),
        liftA4
          MGDNet
          garbageNetKind
          (sampleBernoulli _goSignedness)
          ( sampleMaybe _goOptionalElement $
              mkpair
                (sampleMaybeEnum _goVectoring)
                garbageRange2
          )
          $ sampleMaybe _goOptionalElement garbageDelay3,
        pure MGDGenVar,
        liftA4
          (\(p, l, d) a b -> MGDTask a b p l d)
          garbageStdBlockDecl
          (sampleBernoulli _goAutomatic)
          (sampleN _goArguments $ garbageAttrIded $ mkpair (sampleEnum _goDirection) garbageTFT)
          garbageMybStmt,
        liftA5
          (\(p, lp, d) a b c -> MGDFunc a b c p lp d)
          garbageStdBlockDecl
          (sampleBernoulli _goAutomatic)
          (sampleMaybe _goOptionalElement garbageFPT)
          (toList <$> sampleNE _goArguments (garbageAttrIded garbageTFT))
          garbageStatement
      ]

garbagePortAssign :: GenM' PortAssign
garbagePortAssign =
  choice
    _goNamed_Positional
    (PortNamed <$> sampleN _goArguments (garbageAttrIded $ sampleMaybe _goOptionalElement garbageExpr))
    ( PortPositional
        <$> sampleN
          _goArguments
          (garbageAttributed $ sampleMaybe _goOptionalElement garbageExpr)
    )

garbageParamAssign :: GenM' ParamAssign
garbageParamAssign =
  choice
    _goNamed_Positional
    ( ParamNamed
        <$> sampleN
          _goArguments
          (garbageIdentified $ sampleMaybe _goOptionalElement garbageMinTypMax)
    )
    (ParamPositional <$> sampleN _goArguments garbageExpr)

-- do not generate unknown instantiations, there is no need to
garbageModGenItem :: GenM' (Attributed ModGenItem)
garbageModGenItem =
  garbageAttributed $
    sampleBranch
      _goModGenItem
      [ MGIInitial <$> garbageAttrStmt,
        MGIAlways <$> garbageAttrStmt,
        liftA3 MGIContAss garbageDriveStrength optd3 garbageNetAssign,
        liftA2 (uncurry MGIGateInst) optname $
          sampleBranch
            _goGate
            [ GICMos <$> grev <*> optd3 <*> garbageNetLV <*> garbageExpr <*> garbageExpr <*> garbageExpr,
              GIEnable <$> grev <*> g1_0 <*> garbageDriveStrength <*> optd3
                <*> garbageNetLV
                <*> garbageExpr
                <*> garbageExpr,
              GIMos <$> grev <*> g1_0 <*> optd3 <*> garbageNetLV <*> garbageExpr <*> garbageExpr,
              GINIn <$> sampleEnum _goNInpGate <*> grev <*> garbageDriveStrength <*> optd2
                <*> garbageNetLV
                <*> sampleNE _goArguments garbageExpr,
              liftA5 GINOut grev garbageDriveStrength optd2 (sampleNE _goArguments garbageNetLV) garbageExpr,
              GIPassEn <$> grev <*> g1_0 <*> optd2 <*> garbageNetLV <*> garbageNetLV <*> garbageExpr,
              liftA3 GIPass grev garbageNetLV garbageNetLV,
              liftA3 GIPull grev garbageDriveStrength garbageNetLV
            ],
        liftA4 uncurry (liftA3 MGIUDPInst garbageIdent garbageDriveStrength optd2) optname garbageNetLV $
          sampleNE _goArguments garbageExpr,
        liftA5 MGIModInst garbageIdent garbageParamAssign garbageIdent optr2 garbagePortAssign,
        liftA4
          MGILoopGen
          (garbageIdentified garbageCExpr)
          garbageCExpr
          (garbageIdentified garbageCExpr)
          garbageGenerateBlock,
        liftA3 MGIIf garbageCExpr optblock optblock,
        do
          d <- optblock
          n <- (if d == Nothing then succ else id) <$> sampleNum _goCaseBranches
          liftA2 (\a b -> MGICase a b d) garbageCExpr $
            sequence $
              replicate n $
                liftA2 GenCaseItem (sampleNE _goArguments garbageCExpr) optblock
      ]
  where
    grev = sampleBernoulli _goGateReverse
    g1_0 = sampleBernoulli _goGate1_0
    optd3 = sampleMaybe _goOptionalElement garbageDelay3
    optd2 = sampleMaybe _goOptionalElement garbageDelay2
    optr2 = sampleMaybe _goOptionalElement garbageRange2
    optblock = sampleMaybe _goOptionalElement garbageGenerateBlock
    optname = choice _goOptionalElement (pure ("", Nothing)) (mkpair garbageIdent optr2)

garbageGenerateItem :: GenM' GenerateItem
garbageGenerateItem =
  garbageAttributes >>= \a ->
    sampleBranch
      _goGenerateItem
      [ garbageFPT >>= \t ->
          GIParam
            <$> sampleNE
              _goLocalParameters
              (liftA2 (flip (Parameter a) t) garbageIdent garbageCMinTypMax),
        GIParamOver
          <$> sampleNE
            _goParameterOverrides
            (liftA2 (ParamOver a) garbageHierIdent garbageCMinTypMax),
        GIMGD
          <$> sampleBranch
            _goModGenDeclaration
            [ MGDBlockDecl
                <$> garbageBlockDecl (sampleEither _goDimension_Initialisation garbageDims garbageCExpr)
                >>= \bd -> sdecl $ (\s -> AttrIded a s bd) <$> garbageIdent,
              do
                sn <- sampleBernoulli _goSignedness
                vs <-
                  sampleMaybe _goOptionalElement $
                    mkpair (sampleMaybeEnum _goVectoring) garbageRange2
                d3 <- optd3
                nt <- sampleBernoulli _goNet_Tri
                di <- sampleBernoulli _goDimension_Initialisation
                nk <- case (nt, di) of
                  (True, True) -> (\nt -> NKNet nt . Left <$> garbageDims) <$> sampleEnum _goNetType
                  (True, False) ->
                    liftA2
                      (\nt ds -> NKNet nt . Right . (,) ds <$> garbageExpr)
                      (sampleEnum _goNetType)
                      garbageDriveStrength
                  (False, True) -> (\cs -> NKTriC cs <$> garbageDims) <$> sampleEnum _goChargeStrength
                  (False, False) -> (\ds -> NKTriD ds <$> garbageExpr) <$> garbageDriveStrength
                sdecl $ liftA2 (\s nk -> AttrIded a s $ MGDNet nk sn vs d3) garbageIdent nk,
              sdecl $ flip (AttrIded a) MGDGenVar <$> garbageIdent,
              liftA5
                (\(p, l, d) b c e -> (:| []) . AttrIded a b . MGDTask c e p l d)
                garbageStdBlockDecl
                garbageIdent
                (sampleBernoulli _goAutomatic)
                (sampleN _goArguments (garbageAttrIded $ mkpair (sampleEnum _goDirection) garbageTFT))
                garbageMybStmt,
              (\(p, lp, d) b c e f -> (:| []) . AttrIded a b . MGDFunc c e f p lp d) <$> garbageStdBlockDecl
                <*> garbageIdent
                <*> sampleBernoulli _goAutomatic
                <*> sampleMaybe _goOptionalElement garbageFPT
                <*> (toList <$> sampleNE _goArguments (garbageAttrIded garbageTFT))
                <*> garbageStatement
            ],
        GIMGI
          <$> sampleBranch
            _goModGenItem
            [ singleattr a $ MGIInitial <$> garbageAttrStmt,
              singleattr a $ MGIAlways <$> garbageAttrStmt,
              sitem a $ liftA2 (\ds d3 -> MGIContAss ds d3 <$> garbageNetAssign) garbageDriveStrength optd3,
              sitem a $
                sampleBranch
                  _goGate
                  [ liftA2
                      ( \r d3 ->
                          la2u MGIGateInst $
                            liftA4 (GICMos r d3) garbageNetLV garbageExpr garbageExpr garbageExpr
                      )
                      grev
                      optd3,
                    liftA4
                      ( \r b ds d3 ->
                          la2u MGIGateInst $
                            liftA3 (GIEnable r b ds d3) garbageNetLV garbageExpr garbageExpr
                      )
                      grev
                      g1_0
                      garbageDriveStrength
                      optd3,
                    liftA3
                      ( \r b d3 ->
                          la2u MGIGateInst $
                            liftA3 (GIMos r b d3) garbageNetLV garbageExpr garbageExpr
                      )
                      grev
                      g1_0
                      optd3,
                    liftA4
                      ( \nin r ds d2 ->
                          la2u MGIGateInst $
                            liftA2 (GINIn nin r ds d2) garbageNetLV (sarg garbageExpr)
                      )
                      (sampleEnum _goNInpGate)
                      grev
                      garbageDriveStrength
                      optd2,
                    liftA3
                      ( \r ds d2 ->
                          la2u MGIGateInst $
                            liftA2 (GINOut r ds d2) (sarg garbageNetLV) garbageExpr
                      )
                      grev
                      garbageDriveStrength
                      optd2,
                    liftA3
                      ( \r b d2 ->
                          la2u MGIGateInst $
                            liftA3 (GIPassEn r b d2) garbageNetLV garbageNetLV garbageExpr
                      )
                      grev
                      g1_0
                      optd2,
                    (\r -> la2u MGIGateInst $ liftA2 (GIPass r) garbageNetLV garbageNetLV) <$> grev,
                    liftA2 (\r ds -> la2u MGIGateInst $ GIPull r ds <$> garbageNetLV) grev garbageDriveStrength
                  ],
              sitem a $
                liftA3
                  ( \k ds d2 ->
                      liftA3
                        (uncurry $ MGIUDPInst k ds d2)
                        optname
                        garbageNetLV
                        $ sarg garbageExpr
                  )
                  garbageIdent
                  garbageDriveStrength
                  optd2,
              sitem a $
                liftA2
                  (\k p -> liftA3 (MGIModInst k p) garbageIdent optr2 garbagePortAssign)
                  garbageIdent
                  garbageParamAssign,
              singleattr a $
                liftA4
                  MGILoopGen
                  (garbageIdentified garbageCExpr)
                  garbageCExpr
                  (garbageIdentified garbageCExpr)
                  garbageGenerateBlock,
              singleattr a $ liftA3 MGIIf garbageCExpr optblock optblock,
              singleattr a $ do
                d <- optblock
                n <- (if d == Nothing then succ else id) <$> sampleNum _goCaseBranches
                liftA3
                  MGICase
                  garbageCExpr
                  ( sequence $
                      replicate n $
                        liftA2 GenCaseItem (sarg garbageCExpr) optblock
                  )
                  (pure d)
            ]
      ]
  where
    la2u c = liftA2 (uncurry c) optname
    sdecl = sampleNE _goDeclarations
    sarg = sampleNE _goArguments
    sitem a m = m >>= sampleNE _goOtherItems . fmap (Attributed a)
    singleattr a = fmap $ (:| []) . Attributed a
    grev = sampleBernoulli _goGateReverse
    g1_0 = sampleBernoulli _goGate1_0
    optd3 = sampleMaybe _goOptionalElement garbageDelay3
    optd2 = sampleMaybe _goOptionalElement garbageDelay2
    optr2 = sampleMaybe _goOptionalElement garbageRange2
    optblock = sampleMaybe _goOptionalElement garbageGenerateBlock
    optname = choice _goOptionalElement (pure ("", Nothing)) (mkpair garbageIdent optr2)

garbageGenerateRegion :: GenM' GenerateRegion
garbageGenerateRegion =
  liftA4
    GenerateRegion
    (sampleN _goLocalParameters garbageParameter)
    (sampleN _goParameterOverrides garbageParamOver)
    (sampleN _goDeclarations garbageModGenDecl)
    (sampleN _goOtherItems garbageModGenItem)

garbageGenerateBlock :: GenM' GenerateBlock
garbageGenerateBlock =
  choice
    _goGenerateSingle_Block
    (GBSingle <$> garbageGenerateItem)
    (GBBlock <$> garbageIdentified garbageGenerateRegion)

garbageSpecTerm :: GenM' SpecTerm
garbageSpecTerm = garbageIdentified $ sampleMaybe _goOptionalElement garbageCRangeExpr

garbageSpecParam :: GenM' SpecParam
garbageSpecParam =
  liftA2 SpecParam (sampleMaybe _goOptionalElement garbageRange2) $
    choice _goInitialisation_PathPulse (SPDAssign <$> garbageIdentified garbageCMinTypMax) $
      liftA4 SPDPathPulse garbageSpecTerm garbageSpecTerm garbageCMinTypMax garbageCMinTypMax

garbageSTC :: GenM' SystemTimingCheck
garbageSTC =
  sampleBranch
    _goSystemTimingCheck
    [ STCSetup <$> gstca,
      STCHold <$> gstca,
      liftA2 STCSetupHold gstca gstcaa,
      STCRecovery <$> gstca,
      STCRemoval <$> gstca,
      liftA2 STCRecrem gstca gstcaa,
      STCSkew <$> gstca,
      liftA3 STCTimeSkew gstca gmce gmce,
      liftA4 STCFullSkew gstca garbageExpr gmce gmce,
      liftA3 STCPeriod gctce garbageExpr garbageIdent,
      liftA3 (\a b -> uncurry $ STCWidth a b) gctce garbageExpr $
        choice _goOptionalElement (pure (Nothing, "")) $ liftA2 ((,) . Just) garbageCExpr garbageIdent,
      liftA5 STCNoChange gtce gtce garbageMinTypMax garbageMinTypMax garbageIdent
    ]
  where
    gmce = sampleMaybe _goOptionalElement garbageCExpr
    gtcc = mkpair (sampleBernoulli _goTimingCheckNeg_Pos) garbageExpr
    ged =
      (\v -> if VU.or v then v else VU.replicate 10 True)
        <$> VU.replicateM 10 (sampleBernoulli _goTimingTransition)
    gtce =
      liftA3 TimingCheckEvent (sampleMaybe _goOptionalElement ged) garbageSpecTerm $
        sampleMaybe _goOptionalElement gtcc
    gctce =
      liftA3 ControlledTimingCheckEvent ged garbageSpecTerm $
        sampleMaybe _goOptionalElement gtcc
    gstca = liftA4 STCArgs gtce gtce garbageExpr garbageIdent
    gstcaa = liftA5 STCAddArgs garbageExpr gmmtm gmmtm gde gde
    gmmtm = sampleMaybe _goOptionalElement garbageMinTypMax
    gde = garbageIdentified $ sampleMaybe _goOptionalElement garbageCMinTypMax

garbageSpecifyItem :: GenM' SpecifyItem
garbageSpecifyItem =
  sampleBranch
    _goSpecifyItem
    [ SIPulsestyle <$> sampleBernoulli _goPulseEvent_Detect <*> garbageSpecTerm,
      SIShowcancelled <$> sampleBernoulli _goShowCancelled <*> garbageSpecTerm,
      SIPathDeclaration
        <$> sampleBranch
          _goModulePathCondition
          [pure MPCNone, pure MPCAlways, MPCCond <$> garbageGenExpr garbageIdent (pure ())]
        <*> choice
          _goPathFull_Parallel
          ( SPFull <$> sampleNE _goFullPathTerms garbageSpecTerm
              <*> sampleNE _goFullPathTerms garbageSpecTerm
          )
          (liftA2 SPParallel garbageSpecTerm garbageSpecTerm)
        <*> sampleMaybeEnum _goPolarity
        <*> sampleMaybe
          _goOptionalElement
          (mkpair garbageExpr $ sampleMaybeEnum _goEdgeSensitivity)
        <*> liftA2
          (:|)
          garbageCMinTypMax
          ( sampleFrom _goPathDelayCount [0, 1, 2, 5, 11]
              >>= sequence . flip replicate garbageCMinTypMax
          ),
      SISystemTimingCheck <$> garbageSTC
    ]

garbageModuleBlock :: Bool -> GenM' ModuleBlock
garbageModuleBlock ts = do
  portinter <-
    sampleN _goArguments $
      garbageIdentified $
        sampleN _goLValues $
          garbageIdentified $
            sampleMaybe _goOptionalElement garbageCRangeExpr
  -- let ports = ports ^.. each . identData
  ports <-
    sampleN _goArguments $
      garbageAttrIded $
        sampleBranch
          _goPortType
          [ liftA2 PDIn gmnt garbageSR,
            liftA2 PDInOut gmnt garbageSR,
            liftA2 PDOut gmnt garbageSR,
            PDOutReg <$> garbageSR <*> sampleMaybe _goOptionalElement garbageCExpr,
            PDOutVar <$> sampleFrom _goBlockDeclType [True, False]
              <*> sampleMaybe _goOptionalElement garbageCExpr
          ]
  ModuleBlock
    <$> garbageAttributes
    <*> garbageIdent
    <*> pure portinter
    <*> pure ports
    <*> sampleN _goParameters garbageParameter
    <*> sampleN _goLocalParameters garbageParameter
    <*> sampleN _goDeclarations garbageModGenDecl
    <*> sampleN _goSpecifyParameters (garbageAttributed garbageSpecParam)
    <*> sampleN
      _goModuleItems
      ( sampleBranch
          _goModuleItem
          [ MIMGI <$> garbageModGenItem,
            MIGenReg <$> garbageGenerateRegion,
            MISpecBlock <$> sampleN _goSpecifyParameters garbageSpecParam
              <*> sampleN _goSpecifyItems garbageSpecifyItem
          ]
      )
    <*> (if ts then Just <$> (mkpair gts gts) else pure Nothing)
    <*> sampleBernoulli _goModuleCell
    <*> sampleMaybeEnum _goUnconnectedDrive
    <*> gmnt
  where
    gts = sampleSegment _goTimeMagnitude (-15) 2
    gmnt = sampleMaybe _goOptionalElement $ sampleEnum _goNetType

garbagePrimitiveBlock :: GenM' PrimitiveBlock
garbagePrimitiveBlock =
  PrimitiveBlock
    <$> garbageAttributes
    <*> garbageIdent
    <*> gport
    <*> sampleNE _goArguments gport
    <*> choice
      _goSequential_Combinatorial
      ( SeqTable
          <$> sampleEither _goPortInitialisation garbageCExpr garbageAttributes -- no sem
          <*> sampleMaybeEnum _goPrimitiveInitialisation
          <*> sampleNE _goTableRows gseqrow
      )
      (CombTable <$> sampleNE _goTableRows (liftA2 CombRow gnein goutlv))
  where
    ginlv = sampleEnum _goTableInLevel
    goutlv = sampleEnum _goTableOutLevel
    gnein = sampleNE _goArguments ginlv
    glin = sampleN _goArguments ginlv
    gport = mkpair garbageAttributes garbageIdent
    gedgeseq =
      SISeq
        <$> glin
        <*> choice
          _goEdgeSimple
          (liftA2 EdgeDesc ginlv ginlv)
          (EdgePos_neg <$> sampleBernoulli _goEdgePos_Neg)
        <*> glin
    gseqrow =
      SeqRow
        <$> choice _goEdgeSensitiveRow (SIComb <$> gnein) gedgeseq
        <*> ginlv
        <*> sampleMaybe _goOptionalElement goutlv

garbageVerilog2005 :: GenM' Verilog2005
garbageVerilog2005 =
  Verilog2005
    <$> (sampleBernoulli _goOptionalElement >>= sampleN _goModules . garbageModuleBlock)
    <*> sampleN _goParameterOverrides garbageParamOver
    <*> sampleN _goPrimitives garbagePrimitiveBlock
    <*> sampleN
      _goConfigs
      ( ConfigBlock
          <$> garbageIdent
          <*> sampleN _goDesigns gdot1
          <*> sampleN
            _goConfigItems
            ( ConfigItem
                <$> sampleEither _goCell_Inst gdot1 (sampleNE _goPaths garbageIdent)
                <*> choice
                  _goLiblist_Use
                  (LLULiblist <$> gpath)
                  (LLUUse <$> gdot1 <*> sampleBernoulli _goOptionalElement)
            )
          <*> gpath
      )
  where
    gpath = sampleN _goPaths garbageIdent
    gdot1 = liftA2 Dot1Ident (sampleMaybe _goOptionalElement garbageIdent) garbageIdent

runGarbageGeneration :: Config -> IO Verilog2005
runGarbageGeneration c = do
  gen <- maybe createSystemRandom initialize $ c ^. configGarbageGenerator . goSeed
  runReaderT garbageVerilog2005 (defGeneratorOpts, gen)
