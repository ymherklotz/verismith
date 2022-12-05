{-# LANGUAGE ConstraintKinds #-}
-- Module      : Verismith.Verilog2005.Generator
-- Description : AST random generator
-- Copyright   : (c) 2023 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : experimental
-- Portability : POSIX
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Verismith.Verilog2005.Generator
  ( garbageVerilog2005,
    GeneratorOpts,
    GenM,
    NumberProbability,
    CategoricalProbability,
    defGeneratorOpts,
  )
where

import Control.Applicative (liftA2, liftA3)
import Control.Monad.Reader
import Control.Monad.State.Lazy
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w)
import qualified Data.IntMap.Strict as IntMap
import Data.List
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Tuple
import qualified Data.Vector as V
import Data.Vector.Mutable (PrimMonad, PrimState, RealWorld)
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Numeric.Natural
import System.Random.MWC.Probability
import Verismith.Verilog2005.AST
import Verismith.Verilog2005.Lexer
import Verismith.Verilog2005.Utils (liftA4, liftA5, mkpair)

infixl 4 <.>

(<.>) :: (Monad m, Applicative m) => m (a -> m b) -> m a -> m b
(<.>) mf mx = join $ mf <*> mx

icast :: (Integral a, Num b) => a -> b
icast = fromIntegral . toInteger

-- | Geometric distribution expected length to failure probability
-- expected length l is (1 - f) / f where f is the failure probability
-- so the failure probability is 1 / (1 + l)
length2prob :: Int -> Double
length2prob l = 1 / icast (1 + l)

data NumberProbability
  = NPUniform
      { _NPULow :: !Int,
        _NPUHigh :: !Int
      }
  | NPBinomial
      { _NPBOffset :: !Int,
        _NPBTrials :: !Int,
        _NPBPSuccess :: !Double
      }
  | NPNegativeBinomial -- Geometric is negative binomial with 1 failure
      { _NPNBOffset :: !Int,
        _NPNBFailRate :: !Double,
        _NPNBFailure :: !Int
      }
  | NPPoisson
      { _NPPOffset :: !Int,
        _NPPParam :: !Double
      }
  | NPDiscrete ![(Double, Int)] -- Weight, outcome index
  | NPLinearComb ![(Double, NumberProbability)]

data CategoricalProbability
  = CPDiscrete ![Double]
  | CPBiasedUniform
      { _CPBUBiases :: ![(Double, Int)],
        _CPBUUniforWeight :: Double
      }

defCatProb :: CategoricalProbability
defCatProb = CPBiasedUniform [] 1

attenuate :: [(Double, a)] -> Double -> CategoricalProbability -> CategoricalProbability
attenuate l d p = case p of
  CPDiscrete wl -> CPDiscrete $ zipWith (\a w -> w * d ** (fst a)) l wl
  CPBiasedUniform wl wb ->
    let im = IntMap.fromListWith (+) $ map swap wl
     in CPDiscrete $ map (\(k, a) -> IntMap.findWithDefault wb k im * d ** (fst a)) $ zip [0 ..] l

avoid :: [Int] -> Int -> Int
avoid l x = case l of
  h : t | h <= x -> avoid t $ x + 1
  _ -> x

uniq :: Ord b => (a -> b) -> (a -> a -> a) -> [a] -> [a]
uniq f m l = case sortBy (\x y -> compare (f y) (f x)) l of
  [] -> []
  h : t -> toList $ foldl' (\(x :| a) e -> if f x == f e then (m x e) :| a else e :| x : a) (h :| []) t

clean :: Int -> [(Double, Int)] -> [(Double, Int)]
clean t =
  map (\(x, y) -> (x, max 0 y))
    . uniq snd (\(x1, y1) (x2, y2) -> (x1 + x2, y1))
    . filter ((<= t) . snd)

sampleCat :: PrimMonad m => Int -> Gen (PrimState m) -> CategoricalProbability -> m Int
sampleCat t gen d = case d of
  CPDiscrete l -> sample (categorical $ take (t + 1) l) gen
  CPBiasedUniform l b ->
    let ll = clean t l
        uw = icast (t - length ll) * b
     in sample (discrete $ (uw, Nothing) : map (\(x, y) -> (x, Just y)) ll) gen
          >>= maybe (avoid (map snd ll) <$> sample (uniformR (0, t - length ll)) gen) return

sampleIn :: (Functor m, PrimMonad m) => [a] -> Gen (PrimState m) -> CategoricalProbability -> m a
sampleIn l gen d = (V.fromList l V.!) <$> sampleCat (length l - 1) gen d

sampleInString ::
  (Functor m, PrimMonad m) =>
  B.ByteString ->
  Gen (PrimState m) ->
  CategoricalProbability ->
  m Word8
sampleInString s gen d = (B.index s) <$> sampleCat (B.length s - 1) gen d

type GenM = ReaderT GeneratorOpts IO

type MonadGen m = (PrimMonad m, MonadReader GeneratorOpts m, PrimState m ~ RealWorld)

data GeneratorOpts = GeneratorOpts -- TODO LATER: duplicate for each use
  { goGenerator :: !(Gen (PrimState GenM)),
    goRecursiveAttenuation :: !Double,
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
    goUnconnectedDrive1_0 :: !Double,
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
    goVector_Scalar :: !CategoricalProbability,
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

defGeneratorOpts :: IO GeneratorOpts
defGeneratorOpts =
  ( \gen ->
      GeneratorOpts
        { goGenerator = gen,
          goRecursiveAttenuation = 0.5,
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
          goPrimitiveInitialisation = defCatProb,
          goEdgeSensitiveRow = 0.5,
          goTableInLevel = defCatProb,
          goTableOutLevel = defCatProb,
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
          goPortType = defCatProb,
          goModuleCell = 0.5,
          goUnconnectedDrive1_0 = 0.5,
          goTimeMagnitude = defCatProb,
          goSpecifyItems = NPPoisson 0 1,
          goSpecifyItem = defCatProb,
          goSpecifyParameters = NPPoisson 0 1,
          goInitialisation_PathPulse = 0.5,
          goModulePathCondition = defCatProb,
          goPathDelayCount = defCatProb,
          goPathFull_Parallel = 0.5,
          goFullPathTerms = NPPoisson 0 1,
          goPulseEvent_Detect = 0.5,
          goShowCancelled = 0.5,
          goPolarity = 0.5,
          goEdgeSensitivity = defCatProb,
          goSystemTimingCheck = defCatProb,
          goTimingTransition = 0.25,
          goTimingCheckNeg_Pos = 0.5,
          goGenerateSingle_Block = 0.5,
          goGenerateItem = defCatProb,
          goModGenDeclaration = defCatProb,
          goDimension_Initialisation = 0.5,
          goAutomatic = 0.5,
          goModGenItem = defCatProb,
          goStatement = defCatProb,
          goTypeAbstract_Concrete = 0.5,
          goAbstractType = defCatProb,
          goBlockDeclType = defCatProb,
          goNetType = defCatProb,
          goNet_Tri = 0.5,
          goVector_Scalar = defCatProb,
          goRegister = 0.5,
          goDirection = defCatProb,
          goDriveStrength = defCatProb,
          goChargeStrength = defCatProb,
          goNInpGate = defCatProb,
          goLoopStatement = defCatProb,
          goCaseStatement = defCatProb,
          goProcContAssign = defCatProb,
          goPCAVar_Net = 0.5,
          goGate = defCatProb,
          goGateReverse = 0.5,
          goGate1_0 = 0.5,
          goDelayEvent = defCatProb,
          goEvents = NPNegativeBinomial 0 0.5 1,
          goEvent = defCatProb,
          goEventPrefix = defCatProb,
          goNamed_Positional = 0.5,
          goBlockPar_Seq = 0.5,
          goAssignmentBlocking = 0.5,
          goCaseBranches = NPNegativeBinomial 0 0.25 1,
          goLValues = NPNegativeBinomial 0 0.5 1,
          goAttributes = NPNegativeBinomial 0 0.75 1,
          goPaths = NPNegativeBinomial 0 0.75 1,
          goDelay = CPDiscrete [1, 1, 2, 4],
          goMinTypMax = 0.5,
          goRangeExpr = defCatProb,
          goRangeOffsetPos_Neg = 0.5,
          goDimensions = NPNegativeBinomial 0 0.5 1,
          goSignedness = 0.5,
          goExpression = CPDiscrete [2, 2, 2, 1],
          goConcatenations = NPNegativeBinomial 0 (2.0 / 5.0) 1,
          goUnaryOperation = defCatProb,
          goBinaryOperation = defCatProb,
          goPrimary = CPDiscrete [2, 4, 4, 4, 4, 4, 2, 4, 1, 1, 1, 1, 1],
          goIntRealIdent = defCatProb,
          goEscaped_Simple = 0.5,
          goSimpleLetters = NPNegativeBinomial 0 0.125 1,
          goSimpleLetter = defCatProb,
          goEscapedLetters = NPNegativeBinomial 0 0.125 1,
          goEscapedLetter = defCatProb,
          goSystemLetters = NPNegativeBinomial 0 0.125 1,
          goStringCharacters = NPNegativeBinomial 0 0.125 1,
          goStringCharacter = defCatProb,
          goFixed_Floating = 0.5,
          goExponentSign = defCatProb,
          goX_Z = 0.5,
          goBinarySymbols = NPNegativeBinomial 0 0.125 1,
          goBinarySymbol = defCatProb,
          goOctalSymbols = NPNegativeBinomial 0 0.125 1,
          goOctalSymbol = defCatProb,
          goDecimalSymbols = NPNegativeBinomial 0 0.125 1,
          goDecimalSymbol = defCatProb,
          goHexadecimalSymbols = NPNegativeBinomial 0 0.125 1,
          goHexadecimalSymbol = defCatProb
        }
  )
    <$> createSystemRandom

sampleBernoulli :: MonadGen m => (GeneratorOpts -> Double) -> m Bool
sampleBernoulli p = sample <$> (bernoulli <$> asks p) <.> asks goGenerator

choice :: MonadGen m => (GeneratorOpts -> Double) -> m a -> m a -> m a
choice c t f = sampleBernoulli c >>= \b -> if b then t else f

sampleMaybe :: MonadGen m => (GeneratorOpts -> Double) -> m a -> m (Maybe a)
sampleMaybe c x = choice c (Just <$> x) (pure Nothing)

sampleEither :: MonadGen m => (GeneratorOpts -> Double) -> m a -> m b -> m (Either a b)
sampleEither c t f = choice c (Left <$> t) (Right <$> f)

sampleEnum ::
  forall a m.
  (MonadGen m, Bounded a, Enum a) =>
  (GeneratorOpts -> CategoricalProbability) ->
  m a
sampleEnum p = toEnum . (mib +) <$> (sampleCat (mab - mib - 1) <$> asks goGenerator <.> asks p)
  where
    mib = fromEnum (minBound :: a)
    mab = fromEnum (maxBound :: a)

sampleFrom :: MonadGen m => (GeneratorOpts -> CategoricalProbability) -> [a] -> m a
sampleFrom p l = sampleIn l <$> asks goGenerator <.> asks p

sampleFromString ::
  MonadGen m => (GeneratorOpts -> CategoricalProbability) -> B.ByteString -> m Word8
sampleFromString p s = sampleInString s <$> asks goGenerator <.> asks p

sampleBranch :: MonadGen m => (GeneratorOpts -> CategoricalProbability) -> [m a] -> m a
sampleBranch p l = join $ sampleFrom p l

sampleNum :: MonadGen m => (GeneratorOpts -> NumberProbability) -> m Int
sampleNum p = asks goGenerator >>= \gen -> asks p >>= \d -> aux gen d
  where
    aux gen d = case d of
      NPUniform l h -> sample (uniformR (l, h)) gen
      NPBinomial o t f -> (o +) <$> sample (binomial t f) gen
      NPNegativeBinomial o r f -> (o +) <$> sample (negativeBinomial f r) gen
      NPPoisson o p -> (o +) <$> sample (poisson p) gen
      NPDiscrete l -> sample (discrete l) gen
      NPLinearComb l -> sample (discrete l) gen >>= aux gen

sampleN :: MonadGen m => (GeneratorOpts -> NumberProbability) -> m b -> m [b]
sampleN p x = sampleNum p >>= sequence . flip replicate x

sampleNE :: MonadGen m => (GeneratorOpts -> NumberProbability) -> m b -> m (NonEmpty b)
sampleNE p x = liftA2 (:|) x $ sampleN p x

sampleString ::
  MonadGen m =>
  (GeneratorOpts -> NumberProbability) ->
  (GeneratorOpts -> CategoricalProbability) ->
  B.ByteString ->
  m B.ByteString
sampleString np cp s = B.pack <$> sampleN np (sampleFromString cp s)

sampleNEString ::
  MonadGen m =>
  (GeneratorOpts -> NumberProbability) ->
  (GeneratorOpts -> CategoricalProbability) ->
  B.ByteString ->
  m B.ByteString
sampleNEString np cp s = B.pack . toList <$> sampleNE np (sampleFromString cp s)

deleteFirstOrdered :: Ord c => (a -> c) -> (b -> c) -> [a] -> [b] -> [a]
deleteFirstOrdered pa pb la lb = case (la, lb) of
  (ha : ta, hb : tb) -> case compare (pa ha) (pb hb) of
    LT -> ha : deleteFirstOrdered pa pb ta lb
    EQ -> deleteFirstOrdered pa pb ta tb
    GT -> deleteFirstOrdered pa pb la tb
  _ -> la

merge :: Ord a => [a] -> [a] -> [a]
merge la lb = case (la, lb) of
  (ha : ta, hb : tb) -> case compare ha hb of
    LT -> ha : merge ta lb
    EQ -> ha : hb : merge ta tb
    GT -> hb : merge la tb
  (_, []) -> la
  _ -> lb

sampleFiltered :: MonadGen m => (GeneratorOpts -> CategoricalProbability) -> Int -> [Int] -> m Int
sampleFiltered p t l = do
  gen <- asks goGenerator
  d <- asks p
  case d of
    CPDiscrete l ->
      avoid ll
        <$> sample (discrete $ deleteFirstOrdered snd id (zip (take (t + 1) l) [0 .. t]) ll) gen
    CPBiasedUniform l' b ->
      let ll' = deleteFirstOrdered snd id (clean t l') ll
          uw = icast (t - length ll - length ll') * b
       in sample (discrete $ (uw, Nothing) : map (\(x, y) -> (x, Just y)) ll') gen
            >>= maybe
              ( avoid (merge ll $ map snd ll')
                  <$> sample (uniformR (0, t - length ll - length ll')) gen
              )
              return
  where
    ll = sort $ filter (<= t) l

tameRecursion :: MonadGen m => m a -> m a
tameRecursion =
  local (\go -> go {goCurrentAttenuation = goRecursiveAttenuation go * goCurrentAttenuation go})

freshRecursion :: MonadGen m => m a -> m a
freshRecursion = local (\go -> go {goCurrentAttenuation = 1})

sampleAttenuatedBranch ::
  MonadGen m => (GeneratorOpts -> CategoricalProbability) -> [(Double, m a)] -> m a
sampleAttenuatedBranch p l = do
  gen <- asks goGenerator
  d <- asks p
  a <- asks goCurrentAttenuation
  join $ sampleIn (map snd l) gen (attenuate l a d)

idSimpleLetter :: B.ByteString -- 0-9$ are forbidden as first letters
idSimpleLetter = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789$"

digitCharacter :: B.ByteString
digitCharacter = "0123456789"

-- Start of actual generation

garbageIdent :: MonadGen m => m B.ByteString
garbageIdent =
  choice
    goEscaped_Simple
    ( B.pack . (c2w '\\' :)
        <$> sampleN
          goEscapedLetters
          (toEnum <$> sampleFiltered goEscapedLetter 255 (map fromEnum " \t\f\n"))
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

garbageIdentified :: MonadGen m => m x -> m (Identified x)
garbageIdentified = liftA2 Identified garbageIdent

garbageSysIdent :: MonadGen m => m B.ByteString
garbageSysIdent = sampleNEString goSystemLetters goSimpleLetter idSimpleLetter

garbageHierIdent :: MonadGen m => m HierIdent
garbageHierIdent =
  liftA2
    HierIdent
    (sampleN goPaths $ garbageIdentified $ sampleMaybe goOptionalElement garbageCExpr)
    garbageIdent

garbageInteger :: MonadGen m => m Natural
garbageInteger = parseDecimal <$> sampleString goDecimalSymbols goDecimalSymbol digitCharacter

garbageReal :: MonadGen m => m B.ByteString
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

garbageNumIdent :: MonadGen m => m NumIdent
garbageNumIdent =
  sampleBranch
    goIntRealIdent
    [ NINumber <$> garbageInteger,
      NIReal <$> garbageReal,
      NIIdent <$> garbageIdent
    ]

garbagePrim :: MonadGen m => m i -> m r -> m (GenPrim i r)
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
                  "\"" ++ concatMap (\x -> case x of '"' -> "\\\""; '\\' -> "\\\\"; _ -> [x]) s ++ "\""
        )
          <$> sampleN
            goStringCharacters
            (toEnum <$> sampleFiltered goStringCharacter 255 (map fromEnum "\n"))
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
        garbageInteger >>= \sz ->
          liftA2
            (PrimNumber $ if sz == 0 then Nothing else Just sz)
            (sampleBernoulli goSignedness)
            x
      )
    gexpr = garbageGenExpr ident grng
    recurse x = (1, tameRecursion x)

garbageGenExpr :: MonadGen m => m i -> m r -> m (GenExpr i r)
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

garbageGenMinTypMax :: MonadGen m => m e -> m (GenMinTypMax e)
garbageGenMinTypMax gexpr =
  choice goMinTypMax (liftA3 MTMFull gexpr gexpr gexpr) (MTMSingle <$> gexpr)

garbageRange2 :: MonadGen m => m Range2
garbageRange2 = liftA2 Range2 garbageCExpr garbageCExpr

garbageDims :: MonadGen m => m [Range2]
garbageDims = sampleN goDimensions garbageRange2

garbageGenRangeExpr :: MonadGen m => m e -> m (GenRangeExpr e)
garbageGenRangeExpr ge =
  sampleBranch
    goRangeExpr
    [ GRESingle <$> ge,
      GREPair <$> garbageRange2,
      liftA3 GREBaseOff ge (sampleBernoulli goRangeOffsetPos_Neg) garbageCExpr
    ]

garbageGenDimRange :: MonadGen m => m e -> m (GenDimRange e)
garbageGenDimRange ge = liftA2 GenDimRange (sampleN goDimensions ge) (garbageGenRangeExpr ge)

garbageExpr :: MonadGen m => m Expr
garbageExpr =
  Expr
    <$> garbageGenExpr
      (tameRecursion garbageHierIdent)
      (sampleMaybe goOptionalElement garbageDimRange)

garbageCExpr :: MonadGen m => m CExpr
garbageCExpr =
  CExpr <$> garbageGenExpr garbageIdent (sampleMaybe goOptionalElement garbageCRangeExpr)

garbageRangeExpr :: MonadGen m => m RangeExpr
garbageRangeExpr = garbageGenRangeExpr garbageExpr

garbageCRangeExpr :: MonadGen m => m CRangeExpr
garbageCRangeExpr = garbageGenRangeExpr garbageCExpr

garbageDimRange :: MonadGen m => m DimRange
garbageDimRange = DimRange <$> garbageGenDimRange garbageExpr

garbageCDimRange :: MonadGen m => m CDimRange
garbageCDimRange = CDimRange <$> garbageGenDimRange garbageCExpr

garbageMinTypMax :: MonadGen m => m MinTypMax
garbageMinTypMax = garbageGenMinTypMax garbageExpr

garbageCMinTypMax :: MonadGen m => m CMinTypMax
garbageCMinTypMax = garbageGenMinTypMax garbageCExpr

garbageAttributes :: MonadGen m => m [Attribute]
garbageAttributes =
  sampleN goAttributes $ garbageIdentified $ sampleMaybe goOptionalElement garbageCExpr

garbageAttributed :: MonadGen m => m x -> m (Attributed x)
garbageAttributed = liftA2 Attributed garbageAttributes

garbageAttrIded :: MonadGen m => m x -> m (AttrIded x)
garbageAttrIded = liftA3 AttrIded garbageAttributes garbageIdent

garbageDelay1 :: MonadGen m => m Delay1
garbageDelay1 =
  freshRecursion $
    sampleBranch
      goDelay
      [ D1Base <$> garbageNumIdent,
        D11 <$> garbageMinTypMax
      ]

garbageDelay2 :: MonadGen m => m Delay2
garbageDelay2 =
  freshRecursion $
    sampleBranch
      goDelay
      [ D2Base <$> garbageNumIdent,
        D21 <$> garbageMinTypMax,
        liftA2 D22 garbageMinTypMax garbageMinTypMax
      ]

garbageDelay3 :: MonadGen m => m Delay3
garbageDelay3 =
  freshRecursion $
    sampleBranch
      goDelay
      [ D3Base <$> garbageNumIdent,
        D31 <$> garbageMinTypMax,
        liftA2 D32 garbageMinTypMax garbageMinTypMax,
        liftA3 D33 garbageMinTypMax garbageMinTypMax garbageMinTypMax
      ]

garbageLValue :: MonadGen m => m dr -> m (LValue dr)
garbageLValue gdr =
  freshRecursion $
    sampleN goLValues (garbageLValue gdr) >>= \l -> case l of
      [] -> liftA2 LVSingle garbageHierIdent $ sampleMaybe goOptionalElement gdr
      h : t -> return $ LVConcat $ h :| t

garbageNetLV :: MonadGen m => m NetLValue
garbageNetLV = garbageLValue garbageCDimRange

garbageVarLV :: MonadGen m => m VarLValue
garbageVarLV = garbageLValue garbageDimRange

garbageVarAssign :: MonadGen m => m VarAssign
garbageVarAssign = liftA2 VarAssign garbageVarLV $ freshRecursion garbageExpr

garbageNetAssign :: MonadGen m => m NetAssign
garbageNetAssign = liftA2 NetAssign garbageNetLV $ freshRecursion garbageExpr

garbageStatement :: MonadGen m => m Statement
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

garbageMybStmt :: MonadGen m => m MybStmt
garbageMybStmt = garbageAttributed $ sampleMaybe goOptionalElement garbageStatement

garbageAttrStmt :: MonadGen m => m AttrStmt
garbageAttrStmt = garbageAttributed garbageStatement

garbageSR :: MonadGen m => m SignRange
garbageSR =
  liftA2 SignRange (sampleBernoulli goSignedness) (sampleMaybe goOptionalElement garbageRange2)

garbageFPT :: MonadGen m => m FunParType
garbageFPT =
  choice
    goTypeAbstract_Concrete
    (FPTComType <$> sampleEnum goAbstractType)
    (FPTSignRange <$> garbageSR)

garbageParameter :: MonadGen m => m Parameter
garbageParameter = liftA4 Parameter garbageAttributes garbageIdent garbageFPT garbageCMinTypMax

garbageParamOver :: MonadGen m => m ParamOver
garbageParamOver = liftA3 ParamOver garbageAttributes garbageHierIdent garbageCMinTypMax

garbageBlockDecl :: MonadGen m => m t -> m (BlockDecl t)
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

garbageStdBlockDecl :: MonadGen m => m ([Parameter], [Parameter], StdBlockDecl)
garbageStdBlockDecl =
  liftA3
    (,,)
    (sampleN goParameters garbageParameter)
    (sampleN goLocalParameters garbageParameter)
    $ sampleN goDeclarations $ garbageAttrIded $ garbageBlockDecl garbageDims

garbageDriveStrength :: MonadGen m => m DriveStrength
garbageDriveStrength = do
  x <- strall
  y <- strall
  case (x, y) of
    (Just a, Just b) -> return $ DSNormal a b
    (Nothing, Just b) -> return $ DSHighZ False b
    (Just a, Nothing) -> return $ DSHighZ True a
    _ -> garbageDriveStrength
  where
    strall =
      sampleFrom
        goDriveStrength
        [Just StrSupply, Just StrStrong, Just StrPull, Just StrWeak, Nothing]

garbageNetKind :: MonadGen m => m NetKind
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

garbageTFT :: MonadGen m => m TaskFunType
garbageTFT =
  choice
    goTypeAbstract_Concrete
    (TFTComType <$> sampleEnum goAbstractType)
    (liftA2 TFTRegSignRange (sampleBernoulli goRegister) garbageSR)

garbageModGenDecl :: MonadGen m => m (AttrIded ModGenDecl)
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
                (sampleFrom goVector_Scalar [Nothing, Just True, Just False])
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

garbagePortAssign :: MonadGen m => m PortAssign
garbagePortAssign =
  choice
    goNamed_Positional
    (PortNamed <$> sampleN goArguments (garbageAttrIded $ sampleMaybe goOptionalElement garbageExpr))
    ( PortPositional
        <$> sampleN
          goArguments
          (garbageAttributed $ sampleMaybe goOptionalElement garbageExpr)
    )

garbageParamAssign :: MonadGen m => m ParamAssign
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
garbageModGenItem :: MonadGen m => m (Attributed ModGenItem)
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

garbageGenerateItem :: MonadGen m => m GenerateItem
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
                    mkpair (sampleFrom goVector_Scalar [Nothing, Just True, Just False]) garbageRange2
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

garbageGenerateRegion :: MonadGen m => m GenerateRegion
garbageGenerateRegion =
  liftA4
    GenerateRegion
    (sampleN goLocalParameters garbageParameter)
    (sampleN goParameterOverrides garbageParamOver)
    (sampleN goDeclarations garbageModGenDecl)
    (sampleN goOtherItems garbageModGenItem)

garbageGenerateBlock :: MonadGen m => m GenerateBlock
garbageGenerateBlock =
  choice
    goGenerateSingle_Block
    (GBSingle <$> garbageGenerateItem)
    (GBBlock <$> garbageIdentified garbageGenerateRegion)

garbageSpecTerm :: MonadGen m => m SpecTerm
garbageSpecTerm = garbageIdentified $ sampleMaybe goOptionalElement garbageCRangeExpr

garbageSpecParam :: MonadGen m => m SpecParam
garbageSpecParam =
  liftA2 SpecParam (sampleMaybe goOptionalElement garbageRange2) $
    choice goInitialisation_PathPulse (SPDAssign <$> garbageIdentified garbageCMinTypMax) $
      liftA4 SPDPathPulse garbageSpecTerm garbageSpecTerm garbageCMinTypMax garbageCMinTypMax

garbageSTC :: MonadGen m => m SystemTimingCheck
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

garbageModuleBlocks :: MonadGen m => m [ModuleBlock]
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
                              mkpair garbageExpr $
                                sampleFrom goEdgeSensitivity [Nothing, Just True, Just False]
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
        <*> sampleMaybe goOptionalElement (sampleBernoulli goUnconnectedDrive1_0)
        <*> sampleMaybe goOptionalElement (sampleEnum goNetType)
  where
    gts = (2 -) <$> (sampleCat 17 <$> asks goGenerator <.> asks goTimeMagnitude)
    gmnt = sampleMaybe goOptionalElement $ sampleEnum goNetType

garbageVerilog2005 :: MonadGen m => m Verilog2005
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
