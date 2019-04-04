{-|
Module      : VeriFuzz.Verilog.Gen
Description : Various useful generators.
Copyright   : (c) 2019, Yann Herklotz
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Various useful generators.
-}

{-# LANGUAGE TemplateHaskell #-}

module VeriFuzz.Verilog.Gen
    ( -- * Generation methods
      procedural
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
import           VeriFuzz.Config
import           VeriFuzz.Internal
import           VeriFuzz.Verilog.AST
import           VeriFuzz.Verilog.Internal
import           VeriFuzz.Verilog.Mutate

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

choose :: PortType -> Port -> Bool
choose ptype (Port a _ _ _) = ptype == a

select :: PortType -> StateGen Port
select ptype = do
    context <- get
    case filter (choose Reg) $ context ^.. variables . traverse of
        [] -> newPort ptype
        l  -> Hog.element l

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
    expr <- scopedExpr
    p <- newPort Wire
    return $ ContAssign (p ^. portName) expr

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
    fstat <- Hog.maybe $ SeqBlock <$> some statement
    stmntDepth += 1
    return $ CondStmnt (BinOp expr BinEq 0) (Just tstat) fstat

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

always :: StateGen ModItem
always = do
    stat     <- SeqBlock <$> some statement
    eventReg <- select Reg
    return $ Always (EventCtrl (EId (eventReg ^. portName)) (Just stat))

-- | Generate a random module item.
modItem :: StateGen ModItem
modItem = do
    prob <- askProbability
    Hog.frequency
        [ (prob ^. probAssign, ModCA <$> contAssign)
        , (prob ^. probAlways, always)
        ]

moduleName :: Maybe Identifier -> StateGen Identifier
moduleName (Just t) = return t
moduleName Nothing  = gen arb

initialBlock :: StateGen ModItem
initialBlock = do
    context <- get
    let l = filter (choose Reg) $ context ^.. variables . traverse
    return . Initial . SeqBlock $ makeAssign <$> l
    where
        makeAssign p = NonBlockAssign $ Assign (lvalFromPort p) Nothing 0

-- | Generates a module definition randomly. It always has one output port which
-- is set to @y@. The size of @y@ is the total combination of all the locally
-- defined wires, so that it correctly reflects the internal state of the
-- module.
moduleDef :: Maybe Identifier -> StateGen ModDecl
moduleDef top = do
    name     <- moduleName top
    portList <- some $ newPort Wire
    mi       <- some modItem
    context  <- get
    initBlock <- initialBlock
    let local = filter (`notElem` portList) $ context ^. variables
    let size  = sum $ local ^.. traverse . portSize
    let yport = Port Wire False size "y"
    let comb = combineAssigns_ yport local
    return . declareMod local . ModDecl name [yport] portList $ initBlock : mi <> [comb]

-- | Procedural generation method for random Verilog. Uses internal 'Reader' and
-- 'State' to keep track of the current Verilog code structure.
procedural :: Config -> Gen Verilog
procedural config = Verilog . (: []) . Description <$> Hog.resize
    num
    (runReaderT (evalStateT (moduleDef (Just "top")) context) config)
  where
    context = Context [] 0 $ config ^. configProperty . propDepth
    num     = fromIntegral $ config ^. configProperty . propSize
