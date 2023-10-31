-- Module      : Verismith.Verilog2005.Generator
-- Description : AST random generator
-- Copyright   : (c) 2023 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : experimental
-- Portability : POSIX

module Verismith.Verilog2005.Generator
  ( runGarbageGeneration,
    GeneratorOpts,
    GenM,
    defGeneratorOpts,
  )
where

import Control.Applicative (liftA2, liftA3)
import Control.Lens.Combinators
import Control.Monad.Reader
import Control.Monad.State.Lazy
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w)
import qualified Data.IntMap.Strict as IntMap
import Data.List.NonEmpty (NonEmpty (..), toList)
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

attenuate :: [(Double, a)] -> Double -> CategoricalProbability -> CategoricalProbability
attenuate l d p = case p of
  CPDiscrete wl -> CPDiscrete $ zipWith (\a w -> w * d ** (fst a)) l wl
  CPBiasedUniform wl wb ->
    let im = IntMap.fromListWith (+) $ map swap wl
     in CPDiscrete $ map (\(k, a) -> IntMap.findWithDefault wb k im * d ** (fst a)) $ zip [0 ..] l

data GeneratorOpts = GeneratorOpts -- TODO LATER: duplicate OptionalElement for each use
  { goRecursiveAttenuation :: !Double,
    goCurrentAttenuation :: !Double,
    goOptionalElement :: !Double,
    goConfigs :: !NumberProbability,
    goConfigItems :: !NumberProbability,
    goDesigns :: !NumberProbability,
    goCell_Inst :: !Double,
    goLiblist_Use :: !Double,
    goPrimitives :: !NumberProbability,
    goSequential_Combinatorial :: !Double,
    goTableRows :: !NumberProbability,
    goPortInitialisation :: !Double,
    goPrimitiveInitialisation :: !CategoricalProbability,
    goEdgeSensitiveRow :: !Double,
    goTableInLevel :: !CategoricalProbability,
    goTableOutLevel :: !CategoricalProbability,
    goEdgeSimple :: !Double,
    goEdgePos_Neg :: !Double,
    goModules :: !NumberProbability,
    goParameters :: !NumberProbability,
    goLocalParameters :: !NumberProbability,
    goParameterOverrides :: !NumberProbability,
    goDeclarations :: !NumberProbability,
    goOtherItems :: !NumberProbability,
    goArguments :: !NumberProbability,
    goModuleItem :: !CategoricalProbability,
    goPortType :: !CategoricalProbability,
    goModuleCell :: !Double,
    goUnconnectedDrive :: !CategoricalProbability,
    goTimeMagnitude :: !CategoricalProbability,
    goSpecifyItems :: !NumberProbability,
    goSpecifyItem :: !CategoricalProbability,
    goSpecifyParameters :: !NumberProbability,
    goInitialisation_PathPulse :: !Double,
    goModulePathCondition :: !CategoricalProbability,
    goPathDelayCount :: !CategoricalProbability,
    goPathFull_Parallel :: !Double,
    goFullPathTerms :: !NumberProbability,
    goPulseEvent_Detect :: !Double,
    goShowCancelled :: !Double,
    goPolarity :: !Double,
    goEdgeSensitivity :: !CategoricalProbability,
    goSystemTimingCheck :: !CategoricalProbability,
    goTimingTransition :: !Double,
    goTimingCheckNeg_Pos :: !Double,
    goGenerateSingle_Block :: !Double,
    goGenerateItem :: !CategoricalProbability,
    goModGenItem :: !CategoricalProbability,
    goModGenDeclaration :: !CategoricalProbability,
    goDimension_Initialisation :: !Double,
    goAutomatic :: !Double,
    goStatement :: !CategoricalProbability,
    goTypeAbstract_Concrete :: !Double,
    goAbstractType :: !CategoricalProbability,
    goBlockDeclType :: !CategoricalProbability,
    goNetType :: !CategoricalProbability,
    goNet_Tri :: !Double,
    goVectoring :: !CategoricalProbability,
    goRegister :: !Double,
    goDirection :: !CategoricalProbability,
    goDriveStrength :: !CategoricalProbability,
    goChargeStrength :: !CategoricalProbability,
    goNInpGate :: !CategoricalProbability,
    goLoopStatement :: !CategoricalProbability,
    goCaseStatement :: !CategoricalProbability,
    goProcContAssign :: !CategoricalProbability,
    goPCAVar_Net :: !Double,
    goGate :: !CategoricalProbability,
    goGateReverse :: !Double,
    goGate1_0 :: !Double,
    goDelayEvent :: !CategoricalProbability,
    goEvents :: !NumberProbability,
    goEvent :: !CategoricalProbability,
    goEventPrefix :: !CategoricalProbability,
    goNamed_Positional :: !Double,
    goBlockPar_Seq :: !Double,
    goAssignmentBlocking :: !Double,
    goCaseBranches :: !NumberProbability,
    goLValues :: !NumberProbability,
    goAttributes :: !NumberProbability,
    goPaths :: !NumberProbability,
    goDelay :: !CategoricalProbability,
    goMinTypMax :: !Double,
    goRangeExpr :: !CategoricalProbability,
    goRangeOffsetPos_Neg :: !Double,
    goDimensions :: !NumberProbability,
    goSignedness :: !Double,
    goExpression :: !CategoricalProbability,
    goConcatenations :: !NumberProbability,
    goUnaryOperation :: !CategoricalProbability,
    goBinaryOperation :: !CategoricalProbability,
    goPrimary :: !CategoricalProbability,
    goIntRealIdent :: !CategoricalProbability,
    goEscaped_Simple :: !Double,
    goSimpleLetters :: !NumberProbability,
    goSimpleLetter :: !CategoricalProbability,
    goEscapedLetters :: !NumberProbability,
    goEscapedLetter :: !CategoricalProbability,
    goSystemLetters :: !NumberProbability,
    goStringCharacters :: !NumberProbability,
    goStringCharacter :: !CategoricalProbability,
    goFixed_Floating :: !Double,
    goExponentSign :: !CategoricalProbability,
    goX_Z :: !Double,
    goBinarySymbols :: !NumberProbability,
    goBinarySymbol :: !CategoricalProbability,
    goOctalSymbols :: !NumberProbability,
    goOctalSymbol :: !CategoricalProbability,
    goDecimalSymbols :: !NumberProbability,
    goDecimalSymbol :: !CategoricalProbability,
    goHexadecimalSymbols :: !NumberProbability,
    goHexadecimalSymbol :: !CategoricalProbability
  }

defGeneratorOpts :: GeneratorOpts
defGeneratorOpts =
  GeneratorOpts
    { goRecursiveAttenuation = 0.5,
      goCurrentAttenuation = 1,
      goOptionalElement = 0.5,
      goConfigs = NPPoisson 0 1,
      goConfigItems = NPPoisson 0 1,
      goDesigns = NPPoisson 0 1,
      goCell_Inst = 0.5,
      goLiblist_Use = 0.5,
      goPrimitives = NPPoisson 0 2,
      goSequential_Combinatorial = 0.5,
      goTableRows = NPPoisson 0 4,
      goPortInitialisation = 0.5,
      goPrimitiveInitialisation = uniformCP,
      goEdgeSensitiveRow = 0.5,
      goTableInLevel = uniformCP,
      goTableOutLevel = uniformCP,
      goEdgeSimple = 0.5,
      goEdgePos_Neg = 0.5,
      goModules = NPPoisson 1 2,
      goParameters = NPNegativeBinomial 0 0.5 1,
      goLocalParameters = NPNegativeBinomial 0 0.5 1,
      goParameterOverrides = NPNegativeBinomial 0 0.75 1,
      goDeclarations = NPPoisson 0 1,
      goOtherItems = NPPoisson 0 3,
      goArguments = NPNegativeBinomial 0 (2.0 / 5.0) 1,
      goModuleItem = CPDiscrete [4, 2, 1],
      goPortType = uniformCP,
      goModuleCell = 0.5,
      goUnconnectedDrive = uniformCP,
      goTimeMagnitude = uniformCP,
      goSpecifyItems = NPPoisson 0 1,
      goSpecifyItem = uniformCP,
      goSpecifyParameters = NPPoisson 0 1,
      goInitialisation_PathPulse = 0.5,
      goModulePathCondition = uniformCP,
      goPathDelayCount = uniformCP,
      goPathFull_Parallel = 0.5,
      goFullPathTerms = NPPoisson 0 1,
      goPulseEvent_Detect = 0.5,
      goShowCancelled = 0.5,
      goPolarity = 0.5,
      goEdgeSensitivity = uniformCP,
      goSystemTimingCheck = uniformCP,
      goTimingTransition = 0.25,
      goTimingCheckNeg_Pos = 0.5,
      goGenerateSingle_Block = 0.5,
      goGenerateItem = uniformCP,
      goModGenDeclaration = uniformCP,
      goDimension_Initialisation = 0.5,
      goAutomatic = 0.5,
      goModGenItem = uniformCP,
      goStatement = uniformCP,
      goTypeAbstract_Concrete = 0.5,
      goAbstractType = uniformCP,
      goBlockDeclType = uniformCP,
      goNetType = uniformCP,
      goNet_Tri = 0.5,
      goVectoring = uniformCP,
      goRegister = 0.5,
      goDirection = uniformCP,
      goDriveStrength = uniformCP,
      goChargeStrength = uniformCP,
      goNInpGate = uniformCP,
      goLoopStatement = uniformCP,
      goCaseStatement = uniformCP,
      goProcContAssign = uniformCP,
      goPCAVar_Net = 0.5,
      goGate = uniformCP,
      goGateReverse = 0.5,
      goGate1_0 = 0.5,
      goDelayEvent = uniformCP,
      goEvents = NPNegativeBinomial 0 0.5 1,
      goEvent = uniformCP,
      goEventPrefix = uniformCP,
      goNamed_Positional = 0.5,
      goBlockPar_Seq = 0.5,
      goAssignmentBlocking = 0.5,
      goCaseBranches = NPNegativeBinomial 0 0.25 1,
      goLValues = NPNegativeBinomial 0 0.5 1,
      goAttributes = NPNegativeBinomial 0 0.75 1,
      goPaths = NPNegativeBinomial 0 0.75 1,
      goDelay = CPDiscrete [1, 1, 2, 4],
      goMinTypMax = 0.5,
      goRangeExpr = uniformCP,
      goRangeOffsetPos_Neg = 0.5,
      goDimensions = NPNegativeBinomial 0 0.5 1,
      goSignedness = 0.5,
      goExpression = CPDiscrete [2, 2, 2, 1],
      goConcatenations = NPNegativeBinomial 0 (2.0 / 5.0) 1,
      goUnaryOperation = uniformCP,
      goBinaryOperation = uniformCP,
      goPrimary = CPDiscrete [2, 4, 4, 4, 4, 4, 2, 4, 1, 1, 1, 1, 1],
      goIntRealIdent = uniformCP,
      goEscaped_Simple = 0.5,
      goSimpleLetters = NPNegativeBinomial 0 0.125 1,
      goSimpleLetter = uniformCP,
      goEscapedLetters = NPNegativeBinomial 0 0.125 1,
      goEscapedLetter = uniformCP,
      goSystemLetters = NPNegativeBinomial 0 0.125 1,
      goStringCharacters = NPNegativeBinomial 0 0.125 1,
      goStringCharacter = uniformCP,
      goFixed_Floating = 0.5,
      goExponentSign = uniformCP,
      goX_Z = 0.5,
      goBinarySymbols = NPNegativeBinomial 0 0.125 1,
      goBinarySymbol = uniformCP,
      goOctalSymbols = NPNegativeBinomial 0 0.125 1,
      goOctalSymbol = uniformCP,
      goDecimalSymbols = NPNegativeBinomial 0 0.125 1,
      goDecimalSymbol = uniformCP,
      goHexadecimalSymbols = NPNegativeBinomial 0 0.125 1,
      goHexadecimalSymbol = uniformCP
    }

type GenM' = GenM GeneratorOpts

tameRecursion :: GenM' a -> GenM' a
tameRecursion = local $
  \(go, s) -> (go {goCurrentAttenuation = goRecursiveAttenuation go * goCurrentAttenuation go}, s)

freshRecursion :: GenM' a -> GenM' a
freshRecursion = local $ \(go, s) -> (go {goCurrentAttenuation = 1}, s)

sampleAttenuatedBranch ::
  (GeneratorOpts -> CategoricalProbability) -> [(Double, GenM' a)] -> GenM' a
sampleAttenuatedBranch p l = do
  gen <- asks snd
  d <- asks $ p . fst
  a <- asks $ goCurrentAttenuation . fst
  join $ sampleIn (map snd l) gen (attenuate l a d)

idSimpleLetter :: B.ByteString -- 0-9$ are forbidden as first letters
idSimpleLetter = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789$"

digitCharacter :: B.ByteString
digitCharacter = "0123456789"

-- Start of actual generation

garbageIdent :: GenM' B.ByteString
garbageIdent =
  choice
    goEscaped_Simple
    ( B.pack . (c2w '\\' :)
        <$> sampleN
          goEscapedLetters
          (toEnum <$> sampleSegment goEscapedLetter 33 127)
    )
    ( do
        fl <- sampleFromString goSimpleLetter (B.take 53 idSimpleLetter)
        s <- sampleString goSimpleLetters goSimpleLetter idSimpleLetter
        let loop t =
              let s = B.cons fl t
               in if isKW s
                    then flip B.cons t <$> sampleFromString goSimpleLetter idSimpleLetter >>= loop
                    else return s
        loop s
    )

garbageIdentified :: GenM' x -> GenM' (Identified x)
garbageIdentified = liftA2 Identified garbageIdent

garbageSysIdent :: GenM' B.ByteString
garbageSysIdent = sampleNEString goSystemLetters goSimpleLetter idSimpleLetter

garbageHierIdent :: GenM' HierIdent
garbageHierIdent =
  liftA2
    HierIdent
    (sampleN goPaths $ garbageIdentified $ sampleMaybe goOptionalElement garbageCExpr)
    garbageIdent

garbageInteger :: GenM' Natural
garbageInteger = parseDecimal <$> sampleString goDecimalSymbols goDecimalSymbol digitCharacter

garbageReal :: GenM' B.ByteString
garbageReal =
  choice
    goFixed_Floating
    ( do
        p <- number
        f <- number
        return $ p <> "." <> f
    )
    ( do
        p <- number
        f <- sampleString goDecimalSymbols goDecimalSymbol digitCharacter
        s <- sampleFrom goExponentSign ["", "+", "-"]
        e <- number
        return $ p <> (if B.null f then "" else B.cons (c2w '.') f) <> "e" <> s <> e
    )
  where
    number = sampleNEString goDecimalSymbols goDecimalSymbol digitCharacter

garbageNumIdent :: GenM' NumIdent
garbageNumIdent =
  sampleBranch
    goIntRealIdent
    [ NINumber <$> garbageInteger,
      NIReal <$> garbageReal,
      NIIdent <$> garbageIdent
    ]

garbagePrim :: GenM' i -> GenM' r -> GenM' (GenPrim i r)
garbagePrim ident grng =
  sampleAttenuatedBranch
    goPrimary
    [ mknum $ NXZ <$> sampleBernoulli goX_Z,
      mknum $ NBinary <$> sampleNE goBinarySymbols (sampleEnum goBinarySymbol),
      mknum $ NOctal <$> sampleNE goOctalSymbols (sampleEnum goOctalSymbol),
      mknum $ NDecimal <$> garbageInteger,
      mknum $ NHex <$> sampleNE goHexadecimalSymbols (sampleEnum goHexadecimalSymbol),
      (0, PrimReal <$> garbageReal),
      ( 0,
        ( \s ->
            PrimString $
              B.pack $
                map c2w $
                  "\"" ++ concatMap (\x -> case x of '"' -> "\\\""; '\\' -> "\\\\"; '\n' -> "\\n"; _ -> [x]) s ++ "\""
        )
          <$> sampleN goStringCharacters (sampleEnum goStringCharacter)
      ),
      (0, liftA2 PrimIdent ident $ tameRecursion grng),
      recurse $ PrimConcat <$> sampleNE goConcatenations gexpr,
      recurse $ liftA2 PrimMultConcat garbageCExpr $ sampleNE goConcatenations gexpr,
      recurse $ liftA3 PrimFun ident garbageAttributes $ toList <$> sampleNE goArguments gexpr,
      recurse $ liftA2 PrimSysFun garbageSysIdent $ sampleN goArguments gexpr,
      recurse $ PrimMinTypMax <$> garbageGenMinTypMax gexpr
    ]
  where
    mknum x =
      ( 0,
        do
          sz <- garbageInteger -- TODO: limit size to [0;65535]
          liftA2
            (PrimNumber $ if sz == 0 then Nothing else Just sz)
            (sampleBernoulli goSignedness)
            x
      )
    gexpr = garbageGenExpr ident grng
    recurse x = (1, tameRecursion x)

garbageGenExpr :: GenM' i -> GenM' r -> GenM' (GenExpr i r)
garbageGenExpr ident grng =
  sampleAttenuatedBranch
    goExpression
    [ (0, ExprPrim <$> garbagePrim ident grng),
      ( 0.5,
        tameRecursion $
          liftA3
            ExprUnOp
            (sampleEnum goUnaryOperation)
            garbageAttributes
            $ garbagePrim ident grng
      ),
      ( 1,
        tameRecursion $
          liftA4
            ExprBinOp
            gexpr
            (sampleEnum goBinaryOperation)
            garbageAttributes
            gexpr
      ),
      (2, tameRecursion $ liftA4 ExprCond gexpr garbageAttributes gexpr gexpr)
    ]
  where
    gexpr = garbageGenExpr ident grng

garbageGenMinTypMax :: GenM' e -> GenM' (GenMinTypMax e)
garbageGenMinTypMax gexpr =
  choice goMinTypMax (liftA3 MTMFull gexpr gexpr gexpr) (MTMSingle <$> gexpr)

garbageRange2 :: GenM' Range2
garbageRange2 = liftA2 Range2 garbageCExpr garbageCExpr

garbageDims :: GenM' [Range2]
garbageDims = sampleN goDimensions garbageRange2

garbageGenRangeExpr :: GenM' e -> GenM' (GenRangeExpr e)
garbageGenRangeExpr ge =
  sampleBranch
    goRangeExpr
    [ GRESingle <$> ge,
      GREPair <$> garbageRange2,
      liftA3 GREBaseOff ge (sampleBernoulli goRangeOffsetPos_Neg) garbageCExpr
    ]

garbageGenDimRange :: GenM' e -> GenM' (GenDimRange e)
garbageGenDimRange ge = liftA2 GenDimRange (sampleN goDimensions ge) (garbageGenRangeExpr ge)

garbageExpr :: GenM' Expr
garbageExpr =
  Expr
    <$> garbageGenExpr
      (tameRecursion garbageHierIdent)
      (sampleMaybe goOptionalElement garbageDimRange)

garbageCExpr :: GenM' CExpr
garbageCExpr =
  CExpr <$> garbageGenExpr garbageIdent (sampleMaybe goOptionalElement garbageCRangeExpr)

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
garbageAttributes =
  sampleN goAttributes $ garbageIdentified $ sampleMaybe goOptionalElement garbageCExpr

garbageAttributed :: GenM' x -> GenM' (Attributed x)
garbageAttributed = liftA2 Attributed garbageAttributes

garbageAttrIded :: GenM' x -> GenM' (AttrIded x)
garbageAttrIded = liftA3 AttrIded garbageAttributes garbageIdent

garbageDelay1 :: GenM' Delay1
garbageDelay1 =
  freshRecursion $
    sampleBranch
      goDelay
      [ D1Base <$> garbageNumIdent,
        D11 <$> garbageMinTypMax
      ]

garbageDelay2 :: GenM' Delay2
garbageDelay2 =
  freshRecursion $
    sampleBranch
      goDelay
      [ D2Base <$> garbageNumIdent,
        D21 <$> garbageMinTypMax,
        liftA2 D22 garbageMinTypMax garbageMinTypMax
      ]

garbageDelay3 :: GenM' Delay3
garbageDelay3 =
  freshRecursion $
    sampleBranch
      goDelay
      [ D3Base <$> garbageNumIdent,
        D31 <$> garbageMinTypMax,
        liftA2 D32 garbageMinTypMax garbageMinTypMax,
        liftA3 D33 garbageMinTypMax garbageMinTypMax garbageMinTypMax
      ]

garbageLValue :: GenM' dr -> GenM' (LValue dr)
garbageLValue gdr =
  freshRecursion $
    sampleN goLValues (garbageLValue gdr) >>= \l -> case l of
      [] -> liftA2 LVSingle garbageHierIdent $ sampleMaybe goOptionalElement gdr
      h : t -> return $ LVConcat $ h :| t

garbageNetLV :: GenM' NetLValue
garbageNetLV = garbageLValue garbageCDimRange

garbageVarLV :: GenM' VarLValue
garbageVarLV = garbageLValue garbageDimRange

garbageVarAssign :: GenM' VarAssign
garbageVarAssign = liftA2 VarAssign garbageVarLV $ freshRecursion garbageExpr

garbageNetAssign :: GenM' NetAssign
garbageNetAssign = liftA2 NetAssign garbageNetLV $ freshRecursion garbageExpr

garbageStatement :: GenM' Statement
garbageStatement =
  sampleAttenuatedBranch
    goStatement
    [ ( 0.5,
        liftA2
          SProcTimingControl
          (sampleBranch goDelayEvent [Left <$> garbageDelay1, Right <$> evctl])
          $ tameRecursion garbageMybStmt
      ),
      ( 2,
        liftA3
          SBlock
          ( sampleMaybe
              goOptionalElement
              (liftA2 (\s (p, lp, d) -> (s, p, lp, d)) garbageIdent garbageStdBlockDecl)
          )
          (sampleBernoulli goBlockPar_Seq)
          (sampleN goOtherItems garbageAttrStmt)
      ),
      ( 0,
        liftA3
          SBlockAssign
          (sampleBernoulli goAssignmentBlocking)
          garbageVarAssign
          (sampleMaybe goOptionalElement delevctl)
      ),
      ( 2,
        do
          d <- tameRecursion garbageMybStmt
          n <- (if d == Attributed [] Nothing then succ else id) <$> sampleNum goCaseBranches
          liftA3 (\a b c -> SCase a b c d) (sampleEnum goCaseStatement) gexpr $
            sequence $
              replicate n $
                liftA2 CaseItem (sampleNE goArguments gexpr) (tameRecursion garbageMybStmt)
      ),
      (1, liftA3 SIf gexpr (tameRecursion garbageMybStmt) (tameRecursion garbageMybStmt)),
      ( 0.5,
        liftA2
          SLoop
          ( sampleBranch
              goLoopStatement
              [ pure LSForever,
                LSRepeat <$> gexpr,
                LSWhile <$> gexpr,
                liftA3 LSFor garbageVarAssign gexpr garbageVarAssign
              ]
          )
          (tameRecursion garbageAttrStmt)
      ),
      (0.5, liftA2 SWait gexpr $ tameRecursion garbageMybStmt),
      (0, hie STaskEnable),
      ( 0,
        liftA2 SSysTaskEnable garbageSysIdent $
          sampleN goArguments $ sampleMaybe goOptionalElement gexpr
      ),
      ( 0,
        SProcContAssign
          <$> sampleBranch
            goProcContAssign
            [ PCAAssign <$> garbageVarAssign,
              PCADeassign <$> garbageVarLV,
              PCAForce <$> sampleEither goPCAVar_Net garbageVarAssign garbageNetAssign,
              PCARelease <$> sampleEither goPCAVar_Net garbageVarLV garbageNetLV
            ]
      ),
      (0, SDisable <$> freshRecursion garbageHierIdent),
      (0, hie SEventTrigger)
    ]
  where
    gexpr = freshRecursion garbageExpr
    hie f = freshRecursion $ liftA2 f garbageHierIdent $ sampleN goArguments garbageExpr
    evprm = liftA2 EventPrim (sampleEnum goEventPrefix) gexpr
    evctl =
      sampleBranch
        goEvent
        [ pure ECDeps,
          ECIdent <$> freshRecursion garbageHierIdent,
          ECExpr <$> sampleNE goEvents evprm
        ]
    delevctl =
      sampleBranch
        goDelayEvent
        [ DECDelay <$> garbageDelay1,
          DECEvent <$> evctl,
          liftA2 DECRepeat (freshRecursion garbageExpr) evctl
        ]

garbageMybStmt :: GenM' MybStmt
garbageMybStmt = garbageAttributed $ sampleMaybe goOptionalElement garbageStatement

garbageAttrStmt :: GenM' AttrStmt
garbageAttrStmt = garbageAttributed garbageStatement

garbageSR :: GenM' SignRange
garbageSR =
  liftA2 SignRange (sampleBernoulli goSignedness) (sampleMaybe goOptionalElement garbageRange2)

garbageFPT :: GenM' FunParType
garbageFPT =
  choice
    goTypeAbstract_Concrete
    (FPTComType <$> sampleEnum goAbstractType)
    (FPTSignRange <$> garbageSR)

garbageParameter :: GenM' Parameter
garbageParameter = liftA4 Parameter garbageAttributes garbageIdent garbageFPT garbageCMinTypMax

garbageParamOver :: GenM' ParamOver
garbageParamOver = liftA3 ParamOver garbageAttributes garbageHierIdent garbageCMinTypMax

garbageBlockDecl :: GenM' t -> GenM' (BlockDecl t)
garbageBlockDecl m =
  sampleBranch
    goBlockDeclType
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
    (sampleN goParameters garbageParameter)
    (sampleN goLocalParameters garbageParameter)
    $ sampleN goDeclarations $ garbageAttrIded $ garbageBlockDecl garbageDims

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
    strall = sampleMaybeEnum goDriveStrength

garbageNetKind :: GenM' NetKind
garbageNetKind =
  choice
    goNet_Tri
    ( liftA2 NKNet (sampleEnum goNetType) $
        sampleEither goDimension_Initialisation garbageDims $
          mkpair garbageDriveStrength garbageExpr
    )
    ( choice
        goDimension_Initialisation
        (liftA2 NKTriC (sampleEnum goChargeStrength) garbageDims)
        (liftA2 NKTriD garbageDriveStrength garbageExpr)
    )

garbageTFT :: GenM' TaskFunType
garbageTFT =
  choice
    goTypeAbstract_Concrete
    (TFTComType <$> sampleEnum goAbstractType)
    (liftA2 TFTRegSignRange (sampleBernoulli goRegister) garbageSR)

garbageModGenDecl :: GenM' (AttrIded ModGenDecl)
garbageModGenDecl =
  garbageAttrIded $
    sampleBranch
      goModGenDeclaration
      [ MGDBlockDecl
          <$> garbageBlockDecl (sampleEither goDimension_Initialisation garbageDims garbageCExpr),
        liftA4
          MGDNet
          garbageNetKind
          (sampleBernoulli goSignedness)
          ( sampleMaybe goOptionalElement $
              mkpair
                (sampleMaybeEnum goVectoring)
                garbageRange2
          )
          $ sampleMaybe goOptionalElement garbageDelay3,
        pure MGDGenVar,
        liftA4
          (\(p, l, d) a b -> MGDTask a b p l d)
          garbageStdBlockDecl
          (sampleBernoulli goAutomatic)
          (sampleN goArguments $ garbageAttrIded $ mkpair (sampleEnum goDirection) garbageTFT)
          garbageMybStmt,
        liftA5
          (\(p, lp, d) a b c -> MGDFunc a b c p lp d)
          garbageStdBlockDecl
          (sampleBernoulli goAutomatic)
          (sampleMaybe goOptionalElement garbageFPT)
          (toList <$> sampleNE goArguments (garbageAttrIded garbageTFT))
          garbageStatement
      ]

garbagePortAssign :: GenM' PortAssign
garbagePortAssign =
  choice
    goNamed_Positional
    (PortNamed <$> sampleN goArguments (garbageAttrIded $ sampleMaybe goOptionalElement garbageExpr))
    ( PortPositional
        <$> sampleN
          goArguments
          (garbageAttributed $ sampleMaybe goOptionalElement garbageExpr)
    )

garbageParamAssign :: GenM' ParamAssign
garbageParamAssign =
  choice
    goNamed_Positional
    ( ParamNamed
        <$> sampleN
          goArguments
          (garbageIdentified $ sampleMaybe goOptionalElement garbageMinTypMax)
    )
    (ParamPositional <$> sampleN goArguments garbageExpr)

-- do not generate unknown instantiations, there is no need to
garbageModGenItem :: GenM' (Attributed ModGenItem)
garbageModGenItem =
  garbageAttributed $
    sampleBranch
      goModGenItem
      [ MGIInitial <$> garbageAttrStmt,
        MGIAlways <$> garbageAttrStmt,
        liftA3 MGIContAss garbageDriveStrength optd3 garbageNetAssign,
        liftA2 (uncurry MGIGateInst) optname $
          sampleBranch
            goGate
            [ GICMos <$> grev <*> optd3 <*> garbageNetLV <*> garbageExpr <*> garbageExpr <*> garbageExpr,
              GIEnable <$> grev <*> g1_0 <*> garbageDriveStrength <*> optd3
                <*> garbageNetLV
                <*> garbageExpr
                <*> garbageExpr,
              GIMos <$> grev <*> g1_0 <*> optd3 <*> garbageNetLV <*> garbageExpr <*> garbageExpr,
              GINIn <$> sampleEnum goNInpGate <*> grev <*> garbageDriveStrength <*> optd2
                <*> garbageNetLV
                <*> sampleNE goArguments garbageExpr,
              liftA5 GINOut grev garbageDriveStrength optd2 (sampleNE goArguments garbageNetLV) garbageExpr,
              GIPassEn <$> grev <*> g1_0 <*> optd2 <*> garbageNetLV <*> garbageNetLV <*> garbageExpr,
              liftA3 GIPass grev garbageNetLV garbageNetLV,
              liftA3 GIPull grev garbageDriveStrength garbageNetLV
            ],
        liftA4 uncurry (liftA3 MGIUDPInst garbageIdent garbageDriveStrength optd2) optname garbageNetLV $
          sampleNE goArguments garbageExpr,
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
          n <- (if d == Nothing then succ else id) <$> sampleNum goCaseBranches
          liftA2 (\a b -> MGICase a b d) garbageCExpr $
            sequence $
              replicate n $
                liftA2 GenCaseItem (sampleNE goArguments garbageCExpr) optblock
      ]
  where
    grev = sampleBernoulli goGateReverse
    g1_0 = sampleBernoulli goGate1_0
    optd3 = sampleMaybe goOptionalElement garbageDelay3
    optd2 = sampleMaybe goOptionalElement garbageDelay2
    optr2 = sampleMaybe goOptionalElement garbageRange2
    optblock = sampleMaybe goOptionalElement garbageGenerateBlock
    optname = choice goOptionalElement (pure ("", Nothing)) (mkpair garbageIdent optr2)

garbageGenerateItem :: GenM' GenerateItem
garbageGenerateItem =
  garbageAttributes >>= \a ->
    sampleBranch
      goGenerateItem
      [ garbageFPT >>= \t ->
          GIParam
            <$> sampleNE
              goLocalParameters
              (liftA2 (flip (Parameter a) t) garbageIdent garbageCMinTypMax),
        GIParamOver
          <$> sampleNE
            goParameterOverrides
            (liftA2 (ParamOver a) garbageHierIdent garbageCMinTypMax),
        GIMGD
          <$> sampleBranch
            goModGenDeclaration
            [ MGDBlockDecl
                <$> garbageBlockDecl (sampleEither goDimension_Initialisation garbageDims garbageCExpr)
                >>= \bd -> sdecl $ (\s -> AttrIded a s bd) <$> garbageIdent,
              do
                sn <- sampleBernoulli goSignedness
                vs <-
                  sampleMaybe goOptionalElement $
                    mkpair (sampleMaybeEnum goVectoring) garbageRange2
                d3 <- optd3
                nt <- sampleBernoulli goNet_Tri
                di <- sampleBernoulli goDimension_Initialisation
                nk <- case (nt, di) of
                  (True, True) -> (\nt -> NKNet nt . Left <$> garbageDims) <$> sampleEnum goNetType
                  (True, False) ->
                    liftA2
                      (\nt ds -> NKNet nt . Right . (,) ds <$> garbageExpr)
                      (sampleEnum goNetType)
                      garbageDriveStrength
                  (False, True) -> (\cs -> NKTriC cs <$> garbageDims) <$> sampleEnum goChargeStrength
                  (False, False) -> (\ds -> NKTriD ds <$> garbageExpr) <$> garbageDriveStrength
                sdecl $ liftA2 (\s nk -> AttrIded a s $ MGDNet nk sn vs d3) garbageIdent nk,
              sdecl $ flip (AttrIded a) MGDGenVar <$> garbageIdent,
              liftA5
                (\(p, l, d) b c e -> (:| []) . AttrIded a b . MGDTask c e p l d)
                garbageStdBlockDecl
                garbageIdent
                (sampleBernoulli goAutomatic)
                (sampleN goArguments (garbageAttrIded $ mkpair (sampleEnum goDirection) garbageTFT))
                garbageMybStmt,
              (\(p, lp, d) b c e f -> (:| []) . AttrIded a b . MGDFunc c e f p lp d) <$> garbageStdBlockDecl
                <*> garbageIdent
                <*> sampleBernoulli goAutomatic
                <*> sampleMaybe goOptionalElement garbageFPT
                <*> (toList <$> sampleNE goArguments (garbageAttrIded garbageTFT))
                <*> garbageStatement
            ],
        GIMGI
          <$> sampleBranch
            goModGenItem
            [ singleattr a $ MGIInitial <$> garbageAttrStmt,
              singleattr a $ MGIAlways <$> garbageAttrStmt,
              sitem a $ liftA2 (\ds d3 -> MGIContAss ds d3 <$> garbageNetAssign) garbageDriveStrength optd3,
              sitem a $
                sampleBranch
                  goGate
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
                      (sampleEnum goNInpGate)
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
                n <- (if d == Nothing then succ else id) <$> sampleNum goCaseBranches
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
    sdecl = sampleNE goDeclarations
    sarg = sampleNE goArguments
    sitem a m = m >>= sampleNE goOtherItems . fmap (Attributed a)
    singleattr a = fmap $ (:| []) . Attributed a
    grev = sampleBernoulli goGateReverse
    g1_0 = sampleBernoulli goGate1_0
    optd3 = sampleMaybe goOptionalElement garbageDelay3
    optd2 = sampleMaybe goOptionalElement garbageDelay2
    optr2 = sampleMaybe goOptionalElement garbageRange2
    optblock = sampleMaybe goOptionalElement garbageGenerateBlock
    optname = choice goOptionalElement (pure ("", Nothing)) (mkpair garbageIdent optr2)

garbageGenerateRegion :: GenM' GenerateRegion
garbageGenerateRegion =
  liftA4
    GenerateRegion
    (sampleN goLocalParameters garbageParameter)
    (sampleN goParameterOverrides garbageParamOver)
    (sampleN goDeclarations garbageModGenDecl)
    (sampleN goOtherItems garbageModGenItem)

garbageGenerateBlock :: GenM' GenerateBlock
garbageGenerateBlock =
  choice
    goGenerateSingle_Block
    (GBSingle <$> garbageGenerateItem)
    (GBBlock <$> garbageIdentified garbageGenerateRegion)

garbageSpecTerm :: GenM' SpecTerm
garbageSpecTerm = garbageIdentified $ sampleMaybe goOptionalElement garbageCRangeExpr

garbageSpecParam :: GenM' SpecParam
garbageSpecParam =
  liftA2 SpecParam (sampleMaybe goOptionalElement garbageRange2) $
    choice goInitialisation_PathPulse (SPDAssign <$> garbageIdentified garbageCMinTypMax) $
      liftA4 SPDPathPulse garbageSpecTerm garbageSpecTerm garbageCMinTypMax garbageCMinTypMax

garbageSTC :: GenM' SystemTimingCheck
garbageSTC =
  sampleBranch
    goSystemTimingCheck
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
        choice goOptionalElement (pure (Nothing, "")) $ liftA2 ((,) . Just) garbageCExpr garbageIdent,
      liftA5 STCNoChange gtce gtce garbageMinTypMax garbageMinTypMax garbageIdent
    ]
  where
    gmce = sampleMaybe goOptionalElement garbageCExpr
    gtcc = mkpair (sampleBernoulli goTimingCheckNeg_Pos) garbageExpr
    ged =
      (\v -> if VU.or v then v else VU.replicate 10 True)
        <$> VU.replicateM 10 (sampleBernoulli goTimingTransition)
    gtce =
      liftA3 TimingCheckEvent (sampleMaybe goOptionalElement ged) garbageSpecTerm $
        sampleMaybe goOptionalElement gtcc
    gctce =
      liftA3 ControlledTimingCheckEvent ged garbageSpecTerm $
        sampleMaybe goOptionalElement gtcc
    gstca = liftA4 STCArgs gtce gtce garbageExpr garbageIdent
    gstcaa = liftA5 STCAddArgs garbageExpr gmmtm gmmtm gde gde
    gmmtm = sampleMaybe goOptionalElement garbageMinTypMax
    gde = garbageIdentified $ sampleMaybe goOptionalElement garbageCMinTypMax

garbageModuleBlocks :: GenM' [ModuleBlock]
garbageModuleBlocks =
  sampleBernoulli goOptionalElement >>= \ts ->
    sampleN goModules $
      liftA2 ModuleBlock garbageAttributes garbageIdent
        <*> sampleN
          goArguments
          ( garbageIdentified $
              sampleN goLValues $
                garbageIdentified $ sampleMaybe goOptionalElement garbageCRangeExpr
          )
        <*> sampleN
          goArguments
          ( garbageAttrIded $
              sampleBranch
                goPortType
                [ liftA2 PDIn gmnt garbageSR,
                  liftA2 PDInOut gmnt garbageSR,
                  liftA2 PDOut gmnt garbageSR,
                  liftA2 PDOutReg garbageSR $ sampleMaybe goOptionalElement garbageCExpr,
                  liftA2 PDOutVar (sampleFrom goBlockDeclType [True, False]) $
                    sampleMaybe goOptionalElement garbageCExpr
                ]
          )
        <*> sampleN goParameters garbageParameter
        <*> sampleN goLocalParameters garbageParameter
        <*> sampleN goDeclarations garbageModGenDecl
        <*> sampleN goSpecifyParameters (garbageAttributed garbageSpecParam)
        <*> sampleN
          goOtherItems
          ( sampleBranch
              goModuleItem
              [ MIMGI <$> garbageModGenItem,
                MIGenReg <$> garbageGenerateRegion,
                liftA2 MISpecBlock (sampleN goSpecifyParameters garbageSpecParam) $
                  sampleN goSpecifyItems $
                    sampleBranch
                      goSpecifyItem
                      [ liftA2 SIPulsestyle (sampleBernoulli goPulseEvent_Detect) garbageSpecTerm,
                        liftA2 SIShowcancelled (sampleBernoulli goShowCancelled) garbageSpecTerm,
                        liftA5
                          SIPathDeclaration
                          ( sampleBranch
                              goModulePathCondition
                              [pure MPCNone, pure MPCAlways, MPCCond <$> garbageGenExpr garbageIdent (pure ())]
                          )
                          ( choice
                              goPathFull_Parallel
                              ( liftA2 SPFull (sampleNE goFullPathTerms garbageSpecTerm) $
                                  sampleNE goFullPathTerms garbageSpecTerm
                              )
                              (liftA2 SPParallel garbageSpecTerm garbageSpecTerm)
                          )
                          (sampleMaybe goOptionalElement $ sampleBernoulli goPolarity)
                          ( sampleMaybe goOptionalElement $
                              mkpair garbageExpr $ sampleMaybeEnum goEdgeSensitivity
                          )
                          $ liftA2 (:|) garbageCMinTypMax $
                            sampleFrom goPathDelayCount [0, 1, 2, 5, 11]
                              >>= sequence . flip replicate garbageCMinTypMax,
                        SISystemTimingCheck <$> garbageSTC
                      ]
              ]
          )
        <*> (if ts then Just <$> (mkpair gts gts) else pure Nothing)
        <*> sampleBernoulli goModuleCell
        <*> sampleMaybeEnum goUnconnectedDrive
        <*> sampleMaybe goOptionalElement (sampleEnum goNetType)
  where
    gts = sampleSegment goTimeMagnitude 2 (-15)
    gmnt = sampleMaybe goOptionalElement $ sampleEnum goNetType

garbageVerilog2005 :: GenM' Verilog2005
garbageVerilog2005 =
  liftA4
    Verilog2005
    garbageModuleBlocks
    (sampleN goParameterOverrides garbageParamOver)
    ( sampleN goPrimitives $
        liftA5
          PrimitiveBlock
          garbageAttributes
          garbageIdent
          gport
          (sampleNE goArguments gport)
          $ choice
            goSequential_Combinatorial
            ( liftA3
                SeqTable
                (sampleEither goPortInitialisation garbageCExpr garbageAttributes) -- no sem
                (sampleMaybe goOptionalElement $ sampleEnum goPrimitiveInitialisation)
                $ sampleNE goTableRows $
                  liftA3
                    SeqRow
                    ( choice goEdgeSensitiveRow (SIComb <$> gnein) $
                        liftA3
                          SISeq
                          glin
                          ( choice goEdgeSimple (liftA2 EdgeDesc ginlv ginlv) $
                              EdgePos_neg <$> sampleBernoulli goEdgePos_Neg
                          )
                          glin
                    )
                    ginlv
                    $ sampleMaybe goOptionalElement goutlv
            )
            (CombTable <$> sampleNE goTableRows (liftA2 CombRow gnein goutlv))
    )
    $ sampleN goConfigs $
      liftA4
        ConfigBlock
        garbageIdent
        (sampleN goDesigns gdot1)
        ( sampleN goConfigItems $
            liftA2
              ConfigItem
              (sampleEither goCell_Inst gdot1 $ sampleNE goPaths garbageIdent)
              $ choice goLiblist_Use (LLULiblist <$> gpath) $
                liftA2 LLUUse gdot1 $ sampleBernoulli goOptionalElement
        )
        gpath
  where
    ginlv = sampleEnum goTableInLevel
    goutlv = sampleEnum goTableOutLevel
    gnein = sampleNE goArguments ginlv
    glin = sampleN goArguments ginlv
    gport = mkpair garbageAttributes garbageIdent
    gpath = sampleN goPaths garbageIdent
    gdot1 = liftA2 Dot1Ident garbageIdent $ sampleMaybe goOptionalElement garbageIdent

runGarbageGeneration :: Config -> IO Verilog2005
runGarbageGeneration _c = createSystemRandom >>= runReaderT garbageVerilog2005 . (,) defGeneratorOpts
