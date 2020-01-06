{-|
Module      : Verismith.CounterEg
Description : Counter example parser to load the counter example
Copyright   : (c) 2019, Yann Herklotz
License     : GPL-3
Maintainer  : yann [at] yannherklotz [dot] com
Stability   : experimental
Portability : POSIX
-}

module Verismith.CounterEg
    ( CounterEg(..)
    , parseCounterEg
    )
where

import           Control.Applicative  ((<|>))
import           Data.Bifunctor       (bimap)
import           Data.Binary          (encode)
import           Data.Bits            (shiftL, (.|.))
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import           Data.Char            (digitToInt)
import           Data.Functor         (($>))
import           Data.Maybe           (listToMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Numeric              (readInt)
import qualified Text.Parsec          as P

data CounterEg = CounterEg { _counterEgInitial :: ![(Text, Text)]
                           , _counterEgStates  :: ![[(Text, Text)]]
                           }
               deriving (Eq, Show)

instance Semigroup CounterEg where
    CounterEg a b <> CounterEg c d = CounterEg (a <> c) (b <> d)

instance Monoid CounterEg where
    mempty = CounterEg mempty mempty

type Parser = P.Parsec String ()

-- fromBytes :: ByteString -> Int
-- fromBytes = B.foldl' f 0 where f a b = a `shiftL` 8 .|. fromIntegral b

-- convert :: String -> ByteString
-- convert =
--     L.toStrict
--         . (encode :: Integer -> L.ByteString)
--         . maybe 0 fst
--         . listToMaybe
--         . readInt 2 (`elem` ("01" :: String)) digitToInt

-- convertBinary :: String -> Int
-- convertBinary = fromBytes . convert

lexme :: Parser a -> Parser a
lexme f = do { a <- f; _ <- P.spaces; return a }

nameChar :: Parser Char
nameChar = P.alphaNum
           <|> P.oneOf "$.:_"

parens :: Parser a -> Parser a
parens = lexme . P.between (P.char '(') (P.char ')')

brackets :: Parser a -> Parser a
brackets = lexme . P.between (P.char '[') (P.char ']')

trueOrFalse :: Parser String
trueOrFalse = lexme $ (P.string "true" $> "1") <|> (P.string "false" $> "0")

assumeBody :: Parser (String, String)
assumeBody = lexme $ do
    name <- P.char '=' *> P.spaces *> brackets (P.many1 nameChar)
    num <- P.spaces *> ((P.string "#b" *> P.many1 P.digit) <|> trueOrFalse)
    return (name, num)

parseAssume :: Parser (String, String)
parseAssume = lexme $ P.string "assume" *> P.spaces *> parens assumeBody

parseInitial :: Parser [(String, String)]
parseInitial = lexme $ do
    _ <- lexme $ P.string "initial"
    P.many parseAssume

parseState :: Parser (String, [(String, String)])
parseState = lexme $ do
    n <- lexme $ P.string "state" *> P.spaces *> P.many1 P.digit
    assumes <- P.many parseAssume
    return (n, assumes)

parseCE :: Parser [[(String, String)]]
parseCE = lexme $ do
    i <- parseInitial
    other <- fmap snd <$> P.many parseState
    return (i : other)

cEtoCounterEg :: [[(String, String)]] -> CounterEg
cEtoCounterEg [] = mempty
cEtoCounterEg (i : is) = CounterEg (bimap T.pack T.pack <$> i)
                         (fmap (bimap T.pack T.pack) <$> is)

parseCounterEg' :: Parser CounterEg
parseCounterEg' = lexme $ do
    _ <- P.spaces
    cEtoCounterEg <$> parseCE

parseCounterEg :: Text -> Either String CounterEg
parseCounterEg = bimap show id . P.parse parseCounterEg' "" . T.unpack
