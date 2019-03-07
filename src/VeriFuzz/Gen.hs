{-|
Module      : VeriFuzz.Gen
Description : Various useful generators.
Copyright   : (c) 2019, Yann Herklotz Grave
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
import           Test.QuickCheck                (Gen)
import qualified Test.QuickCheck                as QC
import           VeriFuzz.AST
import           VeriFuzz.ASTGen
import           VeriFuzz.CodeGen
import           VeriFuzz.Config
import           VeriFuzz.Internal
import           VeriFuzz.Mutate
import           VeriFuzz.Random

newtype Context = Context { _variables :: [Port]
                          --                       , _modules   :: [ModDecl]
                          }

makeLenses ''Context

type StateGen =  StateT Context (ReaderT Config Gen)

toId :: Int -> Identifier
toId = Identifier . ("w" <>) . T.pack . show

toPort :: Identifier -> Gen Port
toPort ident = do
    i <- abs <$> QC.arbitrary
    return $ wire i ident

sumSize :: [Port] -> Int
sumSize ps = sum $ ps ^.. traverse . portSize

random :: [Identifier] -> (Expr -> ContAssign) -> Gen ModItem
random ctx fun = do
    expr <- QC.sized (exprWithContext ctx)
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

fromGraph :: Gen ModDecl
fromGraph = do
    gr <- rDupsCirc <$> QC.resize 100 randomCircuit
    return
        $   initMod
        .   head
        $   nestUpTo 5 (generateAST gr)
        ^.. getVerilogSrc
        .   traverse
        .   getDescription

gen :: Gen a -> StateGen a
gen = lift . lift

some :: StateGen a -> StateGen [a]
some f = do
    amount <- gen positiveArb
    replicateM amount f

newPort :: PortType -> StateGen Port
newPort pt = do
    p <- gen $ Port pt <$> QC.arbitrary <*> positiveArb <*> QC.arbitrary
    variables %= (p :)
    return p

contAssign :: StateGen ContAssign
contAssign = do
    p <- newPort Wire
    ContAssign (p ^. portName) <$> scopedExpr

scopedExpr :: StateGen Expr
scopedExpr = do
    context <- get
    gen
        .   QC.sized
        .   exprWithContext
        $   context
        ^.. variables
        .   traverse
        .   portName

lvalFromPort :: Port -> LVal
lvalFromPort (Port _ _ _ i) = RegId i

scopedReg :: StateGen LVal
scopedReg = do
    context <- get
    gen
        .  QC.elements
        .  fmap lvalFromPort
        .  filter (\p -> p ^. portType == Reg)
        $  context
        ^. variables

probability :: Config -> Probability
probability c = c ^. configProbability

askProbability :: StateGen Probability
askProbability = lift $ asks probability

assignment :: StateGen Assign
assignment = do
    lval <- lvalFromPort <$> newPort Reg
    Assign lval Nothing <$> scopedExpr

statement :: StateGen Statement
statement = do
    prob <- askProbability
    as <- assignment
    gen $ QC.frequency
        [ (prob ^. probBlock   , return $ BlockAssign as)
        , (prob ^. probNonBlock, return $ NonBlockAssign as)
        ]

modItem :: StateGen ModItem
modItem = do
    prob      <- askProbability
    amount    <- gen positiveArb
    stat <- fold <$> replicateM amount statement
    event     <- gen QC.arbitrary
    modCA     <- ModCA <$> contAssign
    gen $ QC.frequency
        [ (prob ^. probAssign, return modCA)
        , ( prob ^. probAlways
          , return $ Always (EventCtrl event (Just stat))
          )
        ]

moduleDef :: Bool -> StateGen ModDecl
moduleDef top = do
    name     <- if top then return "top" else gen QC.arbitrary
    portList <- some $ newPort Wire
    amount   <- gen positiveArb
    mi       <- replicateM amount modItem
    context  <- get
    let local = filter (`notElem` portList) $ context ^. variables
    let size  = sum $ local ^.. traverse . portSize
    let yport = Port Wire False size "y"
    return . declareMod local . ModDecl name [yport] portList $ combineAssigns
        yport
        mi

procedural :: Config -> Gen VerilogSrc
procedural config =
    VerilogSrc
        .   (: [])
        .   Description
        <$> QC.resize num (runReaderT (evalStateT (moduleDef True) context) config)
    where context = Context []
          num = config ^. configProperty . propSize
