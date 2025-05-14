{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module      : Verismith.Generate
-- Description : Various useful generators.
-- Copyright   : (c) 2019, Yann Herklotz
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- Various useful generators.
module Verismith.Generate
  ( -- * Generation methods
    procedural,
    proceduralIO,
    proceduralSrc,
    proceduralSrcIO,
    randomMod,

    -- ** Data types
    EMIContext (..),
    emiNewInputs,
    Context (..),
    wires,
    nonblocking,
    blocking,
    outofscope,
    parameters,
    modules,
    nameCounter,
    stmntDepth,
    modDepth,
    determinism,
    emiContext,
    StateGen (..),

    -- ** Generate Functions
    largeNum,
    wireSize,
    range,
    genBitVec,
    binOp,
    unOp,
    constExprWithContext,
    exprSafeList,
    exprRecList,
    exprWithContext,
    makeIdentifier,
    nextWirePort,
    nextNBPort,
    nextBPort,
    newWirePort,
    newNBPort,
    newBPort,
    scopedExpr,
    contAssign,
    lvalFromPort,
    assignment,
    seqBlock,
    conditional,
    forLoop,
    statement,
    alwaysSeq,
    instantiate,
    modInst,
    modItem,
    constExpr,
    parameter,
    moduleDef,

    -- ** Helpers
    someI,
    probability,
    askProbability,
    resizePort,
    moduleName,
    evalRange,
    calcRange,
  )
where

import Control.Lens hiding (Context)
import Control.Monad (replicateM)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Foldable (fold)
import Data.Functor.Foldable (cata)
import Data.List (foldl', partition)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog (Gen, GenT, MonadGen)
import qualified Hedgehog as Hog
import qualified Hedgehog.Gen as Hog
import qualified Hedgehog.Range as Hog
import Verismith.Config
import Verismith.Utils
import Verismith.Verilog.AST
import Verismith.Verilog.BitVec
import Verismith.Verilog.Eval
import Verismith.Verilog.Internal
import Verismith.Verilog.Mutate

data EMIContext = EMIContext
  { _emiNewInputs :: [Port]
  }

makeLenses ''EMIContext

data Context a = Context
  { _wires :: [Port],
    _nonblocking :: [Port],
    _blocking :: [Port],
    _outofscope :: [Port],
    _parameters :: [Parameter],
    _modules :: [ModDecl a],
    _nameCounter :: {-# UNPACK #-} !Int,
    _stmntDepth :: {-# UNPACK #-} !Int,
    _modDepth :: {-# UNPACK #-} !Int,
    _determinism :: !Bool,
    _emiContext :: !(Maybe EMIContext)
  }

makeLenses ''Context

type StateGen a = ReaderT Config (GenT (State (Context a)))

toId :: Int -> Identifier
toId = Identifier . ("w" <>) . T.pack . show

toPort :: (MonadGen m) => Identifier -> m Port
toPort ident = do
  i <- range
  return $ wire i ident

sumSize :: [Port] -> Range
sumSize ps = sum $ ps ^.. traverse . portSize

random :: (MonadGen m) => [Port] -> (Expr -> ContAssign) -> m (ModItem ann)
random ctx fun = do
  expr <- Hog.sized (exprWithContext (ProbExpr 1 1 0 1 1 1 1 0 1 1) [] ctx)
  return . ModCA $ fun expr

-- randomAssigns :: [Identifier] -> [Gen ModItem]
-- randomAssigns ids = random ids . ContAssign <$> ids

randomOrdAssigns :: (MonadGen m) => [Port] -> [Port] -> [m (ModItem ann)]
randomOrdAssigns inp ids = snd $ foldr generate (inp, []) ids
  where
    generate cid (i, o) = (cid : i, random i (ContAssign (_portName cid)) : o)

randomMod :: (MonadGen m) => Int -> Int -> m (ModDecl ann)
randomMod inps total = do
  ident <- sequence $ toPort <$> ids
  x <- sequence $ randomOrdAssigns (start ident) (end ident)
  let inputs_ = take inps ident
  let other = drop inps ident
  let y = ModCA . ContAssign "y" . fold $ Id <$> drop inps ids
  let yport = [wire (sumSize other) "y"]
  return . declareMod other $
    ModDecl
      "test_module"
      yport
      inputs_
      (x ++ [y])
      []
  where
    ids = toId <$> [1 .. total]
    end = drop inps
    start = take inps

-- | Converts a 'Port' to an 'LVal' by only keeping the 'Identifier' of the
-- 'Port'.
lvalFromPort :: Port -> LVal
lvalFromPort (Port _ _ _ i) = RegId i

-- | Returns the probability from the configuration.
probability :: Config -> Probability
probability c = c ^. configProbability

-- | Gets the current probabilities from the 'State'.
askProbability :: StateGen ann Probability
askProbability = asks probability

-- | Generates a random large number, which can also be negative.
largeNum :: (MonadGen m) => m Int
largeNum = Hog.int $ Hog.linear (-100) 100

-- | Generates a random size for a wire so that it is not too small and not too
-- large.
wireSize :: (MonadGen m) => m Int
wireSize = Hog.int $ Hog.linear 2 100

-- | Generates a random range by using the 'wireSize' and 0 as the lower bound.
range :: (MonadGen m) => m Range
range = Range <$> fmap fromIntegral wireSize <*> pure 0

-- | Generate a random bit vector using 'largeNum'.
genBitVec :: (MonadGen m) => m BitVec
genBitVec = fmap fromIntegral largeNum

-- | Return a random 'BinaryOperator'. This currently excludes 'BinDiv',
-- 'BinMod' because they can take a long time to synthesis, and 'BinCEq',
-- 'BinCNEq', because these are not synthesisable. 'BinPower' is also excluded
-- because it can only be used in conjunction with base powers of 2 which is
-- currently not enforced.
binOp :: (MonadGen m) => m BinaryOperator
binOp =
  Hog.element
    [ BinPlus,
      BinMinus,
      BinTimes,
      -- , BinDiv
      -- , BinMod
      BinEq,
      BinNEq,
      -- , BinCEq
      -- , BinCNEq
      BinLAnd,
      BinLOr,
      BinLT,
      BinLEq,
      BinGT,
      BinGEq,
      BinAnd,
      BinOr,
      BinXor,
      BinXNor,
      BinXNorInv,
      -- , BinPower
      BinLSL,
      BinLSR,
      BinASL,
      BinASR
    ]

-- | Generate a random 'UnaryOperator'.
unOp :: (MonadGen m) => m UnaryOperator
unOp =
  Hog.element
    [ UnPlus,
      UnMinus,
      UnNot,
      UnLNot,
      UnAnd,
      UnNand,
      UnOr,
      UnNor,
      UnXor,
      UnNxor,
      UnNxorInv
    ]

-- | Generate a random 'ConstExpr' by using the current context of 'Parameter'.
constExprWithContext :: (MonadGen m) => [Parameter] -> ProbExpr -> Hog.Size -> m ConstExpr
constExprWithContext ps prob size
  | size == 0 =
      Hog.frequency
        [ (prob ^. probExprNum, ConstNum <$> genBitVec),
          ( if null ps then 0 else prob ^. probExprId,
            ParamId . view paramIdent <$> Hog.element ps
          )
        ]
  | size > 0 =
      Hog.frequency
        [ (prob ^. probExprNum, ConstNum <$> genBitVec),
          ( if null ps then 0 else prob ^. probExprId,
            ParamId . view paramIdent <$> Hog.element ps
          ),
          (prob ^. probExprUnOp, ConstUnOp <$> unOp <*> subexpr 2),
          ( prob ^. probExprBinOp,
            ConstBinOp <$> subexpr 2 <*> binOp <*> subexpr 2
          ),
          ( prob ^. probExprCond,
            ConstCond <$> subexpr 2 <*> subexpr 2 <*> subexpr 2
          ),
          ( prob ^. probExprConcat,
            ConstConcat <$> Hog.nonEmpty (Hog.linear 0 10) (subexpr 2)
          )
        ]
  | otherwise = constExprWithContext ps prob 0
  where
    subexpr y = constExprWithContext ps prob $ size `div` y

-- | The list of safe 'Expr', meaning that these will not recurse and will end
-- the 'Expr' generation.
exprSafeList :: (MonadGen m) => ProbExpr -> [(Int, m Expr)]
exprSafeList prob = [(prob ^. probExprNum, Number <$> genBitVec)]

-- | List of 'Expr' that have the chance to recurse and will therefore not be
-- used when the expression grows too large.
exprRecList :: (MonadGen m) => ProbExpr -> (Hog.Size -> m Expr) -> [(Int, m Expr)]
exprRecList prob subexpr =
  [ (prob ^. probExprNum, Number <$> genBitVec),
    ( prob ^. probExprConcat,
      Concat <$> Hog.nonEmpty (Hog.linear 0 10) (subexpr 2)
    ),
    (prob ^. probExprUnOp, UnOp <$> unOp <*> subexpr 2),
    (prob ^. probExprStr, Str <$> Hog.text (Hog.linear 0 100) Hog.alphaNum),
    (prob ^. probExprBinOp, BinOp <$> subexpr 2 <*> binOp <*> subexpr 2),
    (prob ^. probExprCond, Cond <$> subexpr 2 <*> subexpr 2 <*> subexpr 2),
    (prob ^. probExprSigned, Appl <$> pure "$signed" <*> subexpr 2),
    (prob ^. probExprUnsigned, Appl <$> pure "$unsigned" <*> subexpr 2)
  ]

-- | Select a random port from a list of ports and generate a safe bit selection
-- for that port.
rangeSelect :: (MonadGen m) => [Parameter] -> [Port] -> m Expr
rangeSelect ps ports = do
  p <- Hog.element ports
  let s = calcRange ps (Just 32) $ _portSize p
  msb <- Hog.int (Hog.constantFrom (s `div` 2) 0 (s - 1))
  lsb <- Hog.int (Hog.constantFrom (msb `div` 2) 0 msb)
  return . RangeSelect (_portName p) $
    Range
      (fromIntegral msb)
      (fromIntegral lsb)

-- | Generate a random expression from the 'Context' with a guarantee that it
-- will terminate using the list of safe 'Expr'.
exprWithContext :: (MonadGen m) => ProbExpr -> [Parameter] -> [Port] -> Hog.Size -> m Expr
exprWithContext prob ps [] n
  | n == 0 = Hog.frequency $ exprSafeList prob
  | n > 0 = Hog.frequency $ exprRecList prob subexpr
  | otherwise = exprWithContext prob ps [] 0
  where
    subexpr y = exprWithContext prob ps [] $ n `div` y
exprWithContext prob ps l n
  | n == 0 =
      Hog.frequency $
        (prob ^. probExprId, Id . fromPort <$> Hog.element l)
          : exprSafeList prob
  | n > 0 =
      Hog.frequency $
        (prob ^. probExprId, Id . fromPort <$> Hog.element l)
          : (prob ^. probExprRangeSelect, rangeSelect ps l)
          : exprRecList prob subexpr
  | otherwise =
      exprWithContext prob ps l 0
  where
    subexpr y = exprWithContext prob ps l $ n `div` y

-- | Runs a 'StateGen' for a random number of times, limited by an 'Int' that is
-- passed to it.
someI :: Int -> StateGen ann a -> StateGen ann [a]
someI m f = do
  amount <- Hog.int (Hog.linear 1 m)
  replicateM amount f

-- | Make a new name with a prefix and the current nameCounter. The nameCounter
-- is then increased so that the label is unique.
makeIdentifier :: Text -> StateGen ann Identifier
makeIdentifier prefix = do
  context <- get
  let ident = Identifier $ prefix <> showT (context ^. nameCounter)
  nameCounter += 1
  return ident

newPort_ :: Bool -> PortType -> Identifier -> StateGen ann Port
newPort_ blk pt ident = do
  p <- Port pt <$> Hog.bool <*> range <*> pure ident
  case pt of
    Reg -> if blk then blocking %= (p :) else nonblocking %= (p :)
    Wire -> wires %= (p :)
  return p

-- | Creates a new port based on the current name counter and adds it to the
-- current context.  It will be added to the '_wires' list.
newWirePort :: Identifier -> StateGen ann Port
newWirePort = newPort_ False Wire

-- | Creates a new port based on the current name counter and adds it to the
-- current context.  It will be added to the '_nonblocking' list.
newNBPort :: Identifier -> StateGen ann Port
newNBPort = newPort_ False Reg

-- | Creates a new port based on the current name counter and adds it to the
-- current context.  It will be added to the '_blocking' list.
newBPort :: Identifier -> StateGen ann Port
newBPort = newPort_ True Reg

getPort' :: Bool -> PortType -> Identifier -> StateGen ann (Maybe Port)
getPort' blk pt i = do
  cont <- get
  let b = _blocking cont
  let nb = _nonblocking cont
  let w = _wires cont
  let (c, nc) =
        case pt of
          Reg -> if blk then (b, nb <> w) else (nb, b <> w)
          Wire -> (w, b <> nb)
  case (filter portId c, filter portId nc) of
    (_, x : _) -> return Nothing
    (x : _, []) -> return $ Just x
    ([], []) ->
      fmap
        Just
        ( case pt of
            Reg -> if blk then newBPort i else newNBPort i
            Wire -> newWirePort i
        )
  where
    portId (Port pt' _ _ i') = i == i' && pt == pt'

try :: StateGen ann (Maybe a) -> StateGen ann a
try a = do
  r <- a
  case r of
    Nothing -> try a
    Just res -> return res

-- | Makes a new 'Identifier' and then checks if the 'Port' already exists, if
-- it does the existant 'Port' is returned, otherwise a new port is created with
-- 'newPort'. This is used subsequently in all the functions to create a port,
-- in case a port with the same name was already created. This could be because
-- the generation is currently in the other branch of an if-statement.
nextWirePort :: Maybe Text -> StateGen ann Port
nextWirePort i = try $ do
  ident <- makeIdentifier $ fromMaybe (T.toLower $ showT Wire) i
  getPort' False Wire ident

nextNBPort :: Maybe Text -> StateGen ann Port
nextNBPort i = try $ do
  ident <- makeIdentifier $ fromMaybe (T.toLower $ showT Reg) i
  getPort' False Reg ident

nextBPort :: Maybe Text -> StateGen ann Port
nextBPort i = try $ do
  ident <- makeIdentifier $ fromMaybe (T.toLower $ showT Reg) i
  getPort' True Reg ident

allVariables :: StateGen ann [Port]
allVariables =
  fmap (\context -> _wires context <> _nonblocking context <> _blocking context) get

shareableVariables :: StateGen ann [Port]
shareableVariables =
  fmap (\context -> _wires context <> _nonblocking context) get

-- | Generates an expression from variables that are currently in scope.
scopedExpr_ :: [Port] -> StateGen ann Expr
scopedExpr_ vars = do
  context <- get
  prob <- askProbability
  Hog.sized
    . exprWithContext (_probExpr prob) (_parameters context)
    $ vars

scopedExprAll :: StateGen ann Expr
scopedExprAll = allVariables >>= scopedExpr_

scopedExpr :: StateGen ann Expr
scopedExpr = shareableVariables >>= scopedExpr_

-- | Generates a random continuous assignment and assigns it to a random wire
-- that is created.
contAssign :: StateGen ann ContAssign
contAssign = do
  expr <- scopedExpr
  p <- nextWirePort Nothing
  return $ ContAssign (p ^. portName) expr

-- | Generate a random assignment and assign it to a random 'Reg'.
assignment :: Bool -> StateGen ann Assign
assignment blk = do
  expr <- scopedExprAll
  lval <- lvalFromPort <$> (if blk then nextBPort else nextNBPort) Nothing
  return $ Assign lval Nothing expr

-- | Generate a random 'Statement' safely, by also increasing the depth counter.
seqBlock :: StateGen ann (Statement ann)
seqBlock = do
  stmntDepth -= 1
  tstat <- SeqBlock <$> someI 20 statement
  stmntDepth += 1
  return tstat

-- | Generate a random conditional 'Statement'. The nameCounter is reset between
-- branches so that port names can be reused. This is safe because if a 'Port'
-- is not reused, it is left at 0, as all the 'Reg' are initialised to 0 at the
-- start.
conditional :: StateGen ann (Statement ann)
conditional = do
  expr <- scopedExprAll
  nc <- _nameCounter <$> get
  tstat <- seqBlock
  nc' <- _nameCounter <$> get
  nameCounter .= nc
  fstat <- seqBlock
  nc'' <- _nameCounter <$> get
  nameCounter .= max nc' nc''
  return $ CondStmnt expr (Just tstat) (Just fstat)

-- | Generate a random for loop by creating a new variable name for the counter
-- and then generating random statements in the body.
forLoop :: StateGen ann (Statement ann)
forLoop = do
  num <- Hog.int (Hog.linear 0 20)
  var <- lvalFromPort <$> nextBPort (Just "forvar")
  ForLoop
    (Assign var Nothing 0)
    (BinOp (varId var) BinLT $ fromIntegral num)
    (Assign var Nothing $ BinOp (varId var) BinPlus 1)
    <$> seqBlock
  where
    varId v = Id (v ^. regId)

-- | Choose a 'Statement' to generate.
statement :: StateGen ann (Statement ann)
statement = do
  prob <- askProbability
  cont <- get
  let defProb i = prob ^. probStmnt . i
  Hog.frequency
    [ (defProb probStmntBlock, BlockAssign <$> assignment True),
      (defProb probStmntNonBlock, NonBlockAssign <$> assignment False),
      (onDepth cont (defProb probStmntCond), conditional),
      (onDepth cont (defProb probStmntFor), forLoop)
    ]
  where
    onDepth c n = if c ^. stmntDepth > 0 then n else 0

-- | Generate a sequential always block which is dependent on the clock.
alwaysSeq :: StateGen ann (ModItem ann)
alwaysSeq = do
  always <- Always . EventCtrl (EPosEdge "clk") . Just <$> seqBlock
  blk <- fmap _blocking get
  outofscope %= mappend blk
  blocking .= []
  return always

-- | Should resize a port that connects to a module port if the latter is
-- larger.  This should not cause any problems if the same net is used as input
-- multiple times, and is resized multiple times, as it should only get larger.
resizePort :: [Parameter] -> Identifier -> Range -> [Port] -> [Port]
resizePort ps i ra = foldl' func []
  where
    func l p@(Port t _ ri i')
      | i' == i && calc ri < calc ra = (p & portSize .~ ra) : l
      | otherwise = p : l
    calc = calcRange ps $ Just 64

-- | Instantiate a module, where the outputs are new nets that are created, and
-- the inputs are taken from existing ports in the context.
--
-- 1 is subtracted from the inputs for the length because the clock is not
-- counted and is assumed to be there, this should be made nicer by filtering
-- out the clock instead. I think that in general there should be a special
-- representation for the clock.
instantiate :: (ModDecl ann) -> StateGen ann (ModItem ann)
instantiate (ModDecl i outP inP _ _) = do
  vars <- shareableVariables
  outs <- replicateM (length outP) $ nextWirePort Nothing
  ins <- take (length inpFixed) <$> Hog.shuffle vars
  insLit <- replicateM (length inpFixed - length ins) (Number <$> genBitVec)
  mapM_ (uncurry process)
    . zip
      ( zip
          (ins ^.. traverse . portName)
          (ins ^.. traverse . portType)
      )
    $ inpFixed ^.. traverse . portSize
  ident <- makeIdentifier "modinst"
  Hog.choice
    [ return . ModInst i [] ident $ ModConn <$> (toE (outs <> clkPort <> ins) <> insLit),
      ModInst i [] ident
        <$> Hog.shuffle
          ( zipWith
              ModConnNamed
              (view portName <$> outP <> clkPort <> inpFixed)
              (toE (outs <> clkPort <> ins) <> insLit)
          )
    ]
  where
    toE ins = Id . view portName <$> ins
    (inpFixed, clkPort) = partition filterFunc inP
    filterFunc (Port _ _ _ n)
      | n == "clk" = False
      | otherwise = True
    process (p, t) r = do
      params <- view parameters <$> get
      case t of
        Reg -> nonblocking %= resizePort params p r
        Wire -> wires %= resizePort params p r

-- | Generates a module instance by also generating a new module if there are
-- not enough modules currently in the context. It keeps generating new modules
-- for every instance and for every level until either the deepest level is
-- achieved, or the maximum number of modules are reached.
--
-- If the maximum number of levels are reached, it will always pick an instance
-- from the current context. The problem with this approach is that at the end
-- there may be many more than the max amount of modules, as the modules are
-- always set to empty when entering a new level. This is to fix recursive
-- definitions of modules, which are not defined.
--
-- One way to fix that is to also decrement the max modules for every level,
-- depending on how many modules have already been generated. This would mean
-- there would be moments when the module cannot generate a new instance but
-- also not take a module from the current context. A fix for that may be to
-- have a default definition of a simple module that is used instead.
--
-- Another different way to handle this would be to have a probability of taking
-- a module from a context or generating a new one.
modInst :: StateGen ann (ModItem ann)
modInst = do
  prob <- ask
  context <- get
  let maxMods = prob ^. configProperty . propMaxModules
  if length (context ^. modules) < maxMods
    then do
      let currMods = context ^. modules
      let params = context ^. parameters
      let w = _wires context
      let nb = _nonblocking context
      let b = _blocking context
      let oos = _outofscope context
      modules .= []
      wires .= []
      nonblocking .= []
      blocking .= []
      outofscope .= []
      parameters .= []
      modDepth -= 1
      chosenMod <- moduleDef Nothing
      ncont <- get
      let genMods = ncont ^. modules
      modDepth += 1
      parameters .= params
      wires .= w
      nonblocking .= nb
      blocking .= b
      outofscope .= oos
      modules .= chosenMod : currMods <> genMods
      instantiate chosenMod
    else Hog.element (context ^. modules) >>= instantiate

-- | Generate a random module item.
modItem :: StateGen ann (ModItem ann)
modItem = do
  conf <- ask
  let prob = conf ^. configProbability
  context <- get
  let defProb i = prob ^. probModItem . i
  det <-
    Hog.frequency
      [ (conf ^. configProperty . propDeterminism, return True),
        (conf ^. configProperty . propNonDeterminism, return False)
      ]
  determinism .= det
  Hog.frequency
    [ (defProb probModItemAssign, ModCA <$> contAssign),
      (defProb probModItemSeqAlways, alwaysSeq),
      ( if context ^. modDepth > 0 then defProb probModItemInst else 0,
        modInst
      )
    ]

-- | Either return the 'Identifier' that was passed to it, or generate a new
-- 'Identifier' based on the current 'nameCounter'.
moduleName :: Maybe Identifier -> StateGen ann Identifier
moduleName (Just t) = return t
moduleName Nothing = makeIdentifier "module"

-- | Generate a random 'ConstExpr' by using the current context of 'Parameters'.
constExpr :: StateGen ann ConstExpr
constExpr = do
  prob <- askProbability
  context <- get
  Hog.sized $
    constExprWithContext
      (context ^. parameters)
      (prob ^. probExpr)

-- | Generate a random 'Parameter' and assign it to a constant expression which
-- it will be initialised to. The assumption is that this constant expression
-- should always be able to be evaluated with the current context of parameters.
parameter :: StateGen ann Parameter
parameter = do
  ident <- makeIdentifier "param"
  cexpr <- constExpr
  let param = Parameter ident cexpr
  parameters %= (param :)
  return param

-- | Evaluate a range to an integer, and cast it back to a range.
evalRange :: [Parameter] -> Int -> Range -> Range
evalRange ps n (Range l r) = Range (eval l) (eval r)
  where
    eval = ConstNum . cata (evaluateConst ps) . resize n

-- | Calculate a range to an int by maybe resizing the ranges to a value.
calcRange :: [Parameter] -> Maybe Int -> Range -> Int
calcRange ps i (Range l r) = eval l - eval r + 1
  where
    eval a = fromIntegral . cata (evaluateConst ps) $ maybe a (`resize` a) i

-- | Filter out a port based on it's name instead of equality of the ports. This
-- is because the ports might not be equal if the sizes are being updated.
identElem :: Port -> [Port] -> Bool
identElem p = elem (p ^. portName) . toListOf (traverse . portName)

-- | Select items from a list with a specific frequency, returning the new list
-- that contains the selected items. If 0 is passed to both the select and
-- not-select parameter, the function will act like the idententy, returning the
-- original list inside the 'Gen' monad.
--
-- The reason for doing this at the output of a module reduces the number of
-- wires that are exposed at the output and therefore allows the synthesis tool
-- to perform more optimisations that it could otherwise not perform. The
-- synthesis tool is quite strict with optimisations if all the wires and
-- registers are exposed.
selectwfreq :: (MonadGen m) => Int -> Int -> [a] -> m [a]
selectwfreq _ _ [] = return []
selectwfreq s n a@(l : ls)
  | s > 0 && n > 0 =
      Hog.frequency
        [ (s, (l :) <$> selectwfreq s n ls),
          (n, selectwfreq s n ls)
        ]
  | otherwise = return a

-- | Generates a module definition randomly. It always has one output port which
-- is set to @y@. The size of @y@ is the total combination of all the locally
-- defined wires, so that it correctly reflects the internal state of the
-- module.
moduleDef :: Maybe Identifier -> StateGen ann (ModDecl ann)
moduleDef top = do
  name <- moduleName top
  portList <- Hog.list (Hog.linear 4 10) $ nextWirePort Nothing
  mi <- Hog.list (Hog.linear 4 100) modItem
  ps <- Hog.list (Hog.linear 0 10) parameter
  context <- get
  vars <- shareableVariables
  config <- ask
  let (newPorts, local) = partition (`identElem` portList) $ vars <> _outofscope context
  let size =
        evalRange (_parameters context) 32
          . sum
          $ local
            ^.. traverse
              . portSize
  let (ProbMod n s) = config ^. configProbability . probMod
  newlocal <- selectwfreq s n local
  let clock = Port Wire False 1 "clk"
  let combine = config ^. configProperty . propCombine
  let yport =
        if combine then Port Wire False 1 "y" else Port Wire False size "y"
  let comb = combineAssigns_ combine yport newlocal
  return
    . declareMod local
    . ModDecl name [yport] (clock : newPorts) (comb : mi)
    $ ps

-- | Procedural generation method for random Verilog. Uses internal 'Reader' and
-- 'State' to keep track of the current Verilog code structure.
procedural :: Text -> Config -> Gen (Verilog ann)
procedural top config = do
  (mainMod, st) <-
    Hog.resize num $
      runStateT
        (Hog.distributeT (runReaderT (moduleDef (Just $ Identifier top)) config))
        context
  return . Verilog $ mainMod : st ^. modules
  where
    context =
      Context [] [] [] [] [] [] 0 (confProp propStmntDepth) (confProp propModDepth) True Nothing
    num = fromIntegral $ confProp propSize
    confProp i = config ^. configProperty . i

-- | Samples the 'Gen' directly to generate random 'Verilog' using the 'Text' as
-- the name of the main module and the configuration 'Config' to influence the
-- generation.
proceduralIO :: Text -> Config -> IO (Verilog a)
proceduralIO t = Hog.sample . procedural t

-- | Given a 'Text' and a 'Config' will generate a '(SourceInfo ann)' which has the
-- top module set to the right name.
proceduralSrc :: Text -> Config -> Gen (SourceInfo ann)
proceduralSrc t c = SourceInfo t <$> procedural t c

-- | Sampled and wrapped into a '(SourceInfo ann)' with the given top module name.
proceduralSrcIO :: Text -> Config -> IO (SourceInfo ann)
proceduralSrcIO t c = SourceInfo t <$> proceduralIO t c
