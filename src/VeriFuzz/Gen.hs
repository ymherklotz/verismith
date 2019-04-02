{-|
Module      : VeriFuzz.Gen
Description : Various useful generators.
Copyright   : (c) 2019, Yann Herklotz
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Various useful generators.
-}

{-# LANGUAGE TemplateHaskell #-}

module VeriFuzz.Gen
    ( -- * Generation methods
      procedural
    , fromGraph
    , randomMod
    )
where

import           Control.Lens                   hiding (Context)
import           Control.Monad                  (replicateM)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Reader     hiding (local)
import           Control.Monad.Trans.State.Lazy
import           Data.Foldable                  (fold)
import qualified Data.Text                      as T
import           Hedgehog                       (Gen)
import qualified Hedgehog.Gen                   as Hog
import           VeriFuzz.AST
import           VeriFuzz.ASTGen
import           VeriFuzz.Config
import           VeriFuzz.Internal
import           VeriFuzz.Mutate
import           VeriFuzz.Random

data Context = Context { _variables   :: [Port]
                       , _nameCounter :: Int
                       , _stmntDepth  :: Int
                       }

makeLenses ''Context

type StateGen =  StateT Context (ReaderT Config Gen)

toId :: Int -> Identifier
toId = Identifier . ("w" <>) . T.pack . show

toPort :: Identifier -> Gen Port
toPort ident = do
    i <- genPositive
    return $ wire i ident

sumSize :: [Port] -> Int
sumSize ps = sum $ ps ^.. traverse . portSize

random :: [Identifier] -> (Expr -> ContAssign) -> Gen ModItem
random ctx fun = do
    expr <- Hog.sized (exprWithContext ctx)
    return . ModCA $ fun expr

--randomAssigns :: [Identifier] -> [Gen ModItem]
--randomAssigns ids = random ids . ContAssign <$> ids

randomOrdAssigns :: [Identifier] -> [Identifier] -> [Gen ModItem]
randomOrdAssigns inp ids = snd $ foldr generate (inp, []) ids
    where generate cid (i, o) = (cid : i, random i (ContAssign cid) : o)

fromGraph :: Gen ModDecl
fromGraph = do
    gr <- rDupsCirc <$> Hog.resize 100 randomDAG
    return
        $   initMod
        .   head
        $   nestUpTo 5 (generateAST gr)
        ^.. getVerilogSrc
        .   traverse
        .   getDescription

randomMod :: Int -> Int -> Gen ModDecl
randomMod inps total = do
    x     <- sequence $ randomOrdAssigns start end
    ident <- sequence $ toPort <$> ids
    let inputs_ = take inps ident
    let other   = drop inps ident
    let y = ModCA . ContAssign "y" . fold $ Id <$> drop inps ids
    let yport   = [wire (sumSize other) "y"]
    return . declareMod other . ModDecl "test_module" yport inputs_ $ x ++ [y]
  where
    ids   = toId <$> [1 .. total]
    end   = drop inps ids
    start = take inps ids

gen :: Gen a -> StateGen a
gen = lift . lift

some :: StateGen a -> StateGen [a]
some f = do
    amount <- gen genPositive
    replicateM amount f

makeIdentifier :: T.Text -> StateGen Identifier
makeIdentifier prefix = do
    context <- get
    let ident = Identifier $ prefix <> showT (context ^. nameCounter)
    nameCounter += 1
    return ident

newPort :: PortType -> StateGen Port
newPort pt = do
    ident <- makeIdentifier . T.toLower $ showT pt
    p     <- gen $ Port pt <$> arb <*> genPositive <*> pure ident
    variables %= (p :)
    return p

select :: PortType -> StateGen Port
select ptype = do
    context <- get
    case filter chooseReg $ context ^.. variables . traverse of
        [] -> newPort ptype
        l  -> gen $ Hog.element l
    where chooseReg (Port a _ _ _) = ptype == a

scopedExpr :: StateGen Expr
scopedExpr = do
    context <- get
    gen
        .   Hog.sized
        .   exprWithContext
        $   context
        ^.. variables
        .   traverse
        .   portName

contAssign :: StateGen ContAssign
contAssign = do
    p <- newPort Wire
    ContAssign (p ^. portName) <$> scopedExpr

lvalFromPort :: Port -> LVal
lvalFromPort (Port _ _ _ i) = RegId i

probability :: Config -> Probability
probability c = c ^. configProbability

askProbability :: StateGen Probability
askProbability = lift $ asks probability

assignment :: StateGen Assign
assignment = do
    expr <- scopedExpr
    lval <- lvalFromPort <$> newPort Reg
    return $ Assign lval Nothing expr

conditional :: StateGen Statement
conditional = do
    expr <- scopedExpr
    stmntDepth -= 1
    tstat <- SeqBlock <$> some statement
    stmntDepth += 1
    return $ CondStmnt expr (Just tstat) Nothing

statement :: StateGen Statement
statement = do
    prob <- askProbability
    cont <- get
    Hog.frequency
        [ (prob ^. probBlock              , BlockAssign <$> assignment)
        , (prob ^. probNonBlock           , NonBlockAssign <$> assignment)
        , (onDepth cont (prob ^. probCond), conditional)
        ]
    where onDepth c n = if c ^. stmntDepth > 0 then n else 0

-- | Generate a random module item.
modItem :: StateGen ModItem
modItem = do
    prob     <- askProbability
    stat     <- fold <$> some statement
    eventReg <- select Reg
    modCA    <- ModCA <$> contAssign
    gen $ Hog.frequency
        [ (prob ^. probAssign, return modCA)
        , ( prob ^. probAlways
          , return $ Always (EventCtrl (EId (eventReg ^. portName)) (Just stat))
          )
        ]

-- | Generates a module definition randomly. It always has one output port which
-- is set to @y@. The size of @y@ is the total combination of all the locally
-- defined wires, so that it correctly reflects the internal state of the
-- module.
moduleDef :: Bool -> StateGen ModDecl
moduleDef top = do
    name     <- if top then return "top" else gen arb
    portList <- some $ newPort Wire
    mi       <- some modItem
    context  <- get
    let local = filter (`notElem` portList) $ context ^. variables
    let size  = sum $ local ^.. traverse . portSize
    let yport = Port Wire False size "y"
    return . declareMod local . ModDecl name [yport] portList $ combineAssigns
        yport
        mi

-- | Procedural generation method for random Verilog. Uses internal 'Reader' and
-- 'State' to keep track of the current Verilog code structure.
procedural :: Config -> Gen VerilogSrc
procedural config = VerilogSrc . (: []) . Description <$> Hog.resize
    num
    (runReaderT (evalStateT (moduleDef True) context) config)
  where
    context = Context [] 0 $ config ^. configProperty . propDepth
    num     = fromIntegral $ config ^. configProperty . propSize
