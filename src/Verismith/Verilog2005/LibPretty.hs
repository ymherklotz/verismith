{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
-- Module      : Verismith.Verilog2005.LibPretty
-- Description : Pretty printer library with intuitive display algorithm.
-- Copyright   : (c) 2023 Quentin Corradi
-- License     : GPL-3
-- Maintainer  : q [dot] corradi22 [at] imperial [dot] ac [dot] uk
-- Stability   : experimental
-- Portability : POSIX
{-# LANGUAGE OverloadedLists #-}

module Verismith.Verilog2005.LibPretty
  ( Doc,
    nullDoc,
    raw,
    alt,
    viaShow,
    lparen,
    rparen,
    lbrace,
    rbrace,
    lbracket,
    rbracket,
    langle,
    rangle,
    comma,
    colon,
    dot,
    equals,
    semi,
    squote,
    space,
    indent,
    hardline,
    newline,
    softline,
    softspace,
    encl,
    (<+>),
    (<#>),
    (<->),
    (<=>),
    (</>),
    mkopt,
    (<?+>),
    (<?#>),
    (<?=>),
    (<?/>),
    trailoptcat,
    nest,
    group,
    ng,
    block,
    layout,
  )
where

import Control.Applicative (liftA2)
import Data.Bits
import qualified Data.ByteString as SB
import Data.ByteString.Builder
import Data.ByteString.Internal (isSpaceWord8, w2c)
import qualified Data.ByteString.Lazy as LB
import Data.Char
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Semigroup
import Data.String
import Verismith.Utils hiding (comma)

infixr 6 <+>

infixr 6 <#>

infixr 6 <->

infixr 6 <=>

infixr 6 </>

infixr 6 <?+>

infixr 6 <?#>

infixr 6 <?=>

infixr 6 <?/>

-- Consider adding a constructor to keep first and last line in constant time reach
data Doc' w
  = Lines w (Doc' w) (Doc' w)
  | Line Word (Line w)

data Line w
  = Concat w (Line w) (Line w)
  | Token SB.ByteString
  | Group w (Line w)
  | Nest w (Line w)
  | Alt SB.ByteString (Doc' w)

type Doc = Doc' ()

type DocLength w = (Ord w, Semigroup w, Enum w)

emptyLine :: Line w
emptyLine = Token ""

emptyDoc :: Doc' w
emptyDoc = Line 0 emptyLine

nullLine :: Line w -> Bool
nullLine l = case l of
  Concat _ a b -> nullLine a && nullLine b
  Token t -> SB.null t
  Alt t d -> SB.null t && nullDoc d
  Group _ l -> nullLine l
  Nest _ l -> nullLine l

nullDoc :: Doc' w -> Bool
nullDoc d = case d of
  Lines _ _ _ -> False
  Line i l -> i == 0 && nullLine l

linelength :: (Semigroup w, Enum w) => Word -> Line w -> w
linelength i l = toEnum (4 * fromEnum i) <> maxLengthLine l

maxLengthLine :: Enum w => Line w -> w
maxLengthLine l = case l of
  Concat w _ _ -> w
  Token t -> toEnum $ SB.length t
  Alt t _ -> toEnum $ SB.length t
  Group w _ -> w
  Nest w _ -> w

maxLengthDoc :: (Semigroup w, Enum w) => Doc' w -> w
maxLengthDoc d = case d of
  Lines w _ _ -> w
  Line i l -> linelength i l

instance (Semigroup w, Enum w) => Semigroup (Line w) where
  (<>) a b =
    if nullLine a
      then b
      else
        if nullLine b
          then a
          else case (a, b) of
            (Group wa da, Group wb db) -> Group (wa <> wb) (da <> db)
            (_, _) -> Concat (maxLengthLine a <> maxLengthLine b) a b

instance (Semigroup w, Enum w) => Monoid (Line w) where
  mempty = emptyLine

instance IsString (Line w) where
  fromString = Token . fromString

(<#>) :: DocLength w => Doc' w -> Doc' w -> Doc' w
(<#>) a b = Lines (max (maxLengthDoc a) (maxLengthDoc b)) a b

extractFirstLine :: DocLength w => Doc' w -> Doc' w -> (Word, Line w, Doc' w)
extractFirstLine a b = case a of
  Line i l -> (i, l, b)
  Lines _ l a -> extractFirstLine l $ a <#> b

extractLastLine :: DocLength w => Doc' w -> Doc' w -> (Doc' w, Word, Line w)
extractLastLine a b = case b of
  Line i l -> (a, i, l)
  Lines _ b l -> extractLastLine (a <#> b) l

instance DocLength w => Semigroup (Doc' w) where
  (<>) a b = case (a, b) of
    (Line ia la, Line ib lb) -> catline ia la ib lb
    (Line il l, Lines _ a b) -> let (ic, c, r) = extractFirstLine a b in catline il l ic c <#> r
    (Lines w a b, Line ir r) -> let (l, ic, c) = extractLastLine a b in l <#> catline ic c ir r
    (Lines _ a b, Lines _ c d) ->
      let (ll, il, lr) = extractLastLine a b
          (ir, rl, rr) = extractFirstLine c d
       in ll <#> catline il lr ir rl <#> rr
    where
      catline ia la ib lb = if nullLine la then Line (ia + ib) lb else Line ia (la <> lb)

instance DocLength w => Monoid (Doc' w) where
  mempty = emptyDoc

instance DocLength w => IsString (Doc' w) where
  fromString s =
    nonEmpty
      mempty
      ( foldrMap1 f ((<#>) . f)
          . \((h :| l) :| t) -> if h == '\n' then [h] :| (h :| l) : t else ('\n' :| h : l) :| t
      )
      $ NE.groupBy (const (/= '\n')) s
    where
      f s =
        ( \(a, b) ->
            Line (div (foldl' (\b c -> 1 + if c == '\t' then b .|. 3 else b) 0 a) 4) $ fromString b
        )
          $ span isSpace $ NE.tail s

raw :: SB.ByteString -> Doc' w
raw = Line 0 . Token

alt :: SB.ByteString -> Doc' w -> Doc' w
alt a b = Line 0 $ Alt a b

viaShow :: Show a => a -> Doc' w
viaShow = raw . fromString . show

lparen :: Doc' w
lparen = raw "("

rparen :: Doc' w
rparen = raw ")"

lbrace :: Doc' w
lbrace = raw "{"

rbrace :: Doc' w
rbrace = raw "}"

lbracket :: Doc' w
lbracket = raw "["

rbracket :: Doc' w
rbracket = raw "]"

langle :: Doc' w
langle = raw "<"

rangle :: Doc' w
rangle = raw ">"

comma :: Doc' w
comma = raw ","

colon :: Doc' w
colon = raw ":"

dot :: Doc' w
dot = raw "."

equals :: Doc' w
equals = raw "="

semi :: Doc' w
semi = raw ";"

squote :: Doc' w
squote = raw "'"

space :: Doc' w
space = raw " "

hardline :: Enum w => Doc' w
hardline = Lines (toEnum 0) emptyDoc emptyDoc

newline :: Enum w => Doc' w
newline = alt " " hardline

softline :: Enum w => Doc' w
softline = alt "" hardline

softspace :: Doc' w
softspace = alt "" $ raw " "

appendWith :: DocLength w => Line w -> Doc' w -> Doc' w -> Doc' w
appendWith l a b = case (a, b) of
  (Line ia la, Line ib lb) -> Line ia $ la <> l <> lb
  (Line i ll, Lines _ a b) -> let (_, c, r) = extractFirstLine a b in Line i (ll <> l <> c) <#> r
  (Lines _ a b, Line _ r) -> let (ll, i, c) = extractLastLine a b in ll <#> Line i (c <> l <> r)
  (Lines _ a b, Lines _ c d) ->
    let (ll, i, lr) = extractLastLine a b
        (_, rl, rr) = extractFirstLine c d
     in ll <#> Line i (lr <> l <> rl) <#> rr

(<+>) :: DocLength w => Doc' w -> Doc' w -> Doc' w
(<+>) = appendWith " "

(<->) :: DocLength w => Doc' w -> Doc' w -> Doc' w
(<->) = appendWith $ Alt "" $ raw " "

(<=>) :: DocLength w => Doc' w -> Doc' w -> Doc' w
(<=>) = appendWith $ Alt " " hardline

(</>) :: DocLength w => Doc' w -> Doc' w -> Doc' w
(</>) = appendWith $ Alt "" hardline

mkopt :: (Doc' w -> Doc' w -> Doc' w) -> Doc' w -> Doc' w -> Doc' w
mkopt f a b = if nullDoc a then b else if nullDoc b then a else f a b

(<?+>) :: DocLength w => Doc' w -> Doc' w -> Doc' w
(<?+>) = mkopt (<+>)

(<?#>) :: DocLength w => Doc' w -> Doc' w -> Doc' w
(<?#>) = mkopt (<#>)

(<?=>) :: DocLength w => Doc' w -> Doc' w -> Doc' w
(<?=>) = mkopt (<=>)

(<?/>) :: DocLength w => Doc' w -> Doc' w -> Doc' w
(<?/>) = mkopt (</>)

trailoptcat :: Foldable f => (Doc' w -> Doc' w -> Doc' w) -> f (Doc' w) -> Doc' w
trailoptcat f =
  fromMaybe emptyDoc
    . foldr (\a -> maybe (if nullDoc a then Nothing else Just a) $ Just . f a) Nothing

foldDoc :: (Word -> Line w -> a) -> (a -> a -> a) -> Doc' w -> a
foldDoc f g d = case d of Lines _ a b -> g (foldDoc f g a) (foldDoc f g b); Line i l -> f i l

mapDoc :: DocLength w => (Word -> Line v -> Doc' w) -> Doc' v -> Doc' w
mapDoc f = foldDoc f (<#>)

foldLine ::
  (SB.ByteString -> a) ->
  (SB.ByteString -> Doc' w -> a) ->
  (a -> a) ->
  (a -> a) ->
  (a -> a -> a) ->
  Line w ->
  a
foldLine f g h i j l = case l of
  Token s -> f s
  Alt s d -> g s d
  Group _ l -> h $ foldLine f g h i j l
  Nest _ l -> i $ foldLine f g h i j l
  Concat _ a b -> j (foldLine f g h i j a) (foldLine f g h i j b)

group :: DocLength w => Doc' w -> Doc' w
group d = case d of
  Line i l -> Line i $ mk l
  Lines w a b -> let (il, l, r) = extractFirstLine a b in Lines w (Line il $ mk l) r
  where
    mk l = Group (maxLengthLine l) l

indent :: DocLength w => Word -> Doc' w -> Doc' w
indent i = mapDoc $ Line . (i +)

nest :: DocLength w => Doc' w -> Doc' w
nest d = case d of
  Line i l -> Line i $ mk l
  Lines _ a b -> let (il, l, r) = extractFirstLine a b in Line il (mk l) <#> indent 1 r
  where
    mk l = Nest (maxLengthLine l) l

ng :: DocLength w => Doc' w -> Doc' w
ng d = case d of
  Line i l -> Line i $ mk l
  Lines _ a b -> let (il, l, r) = extractFirstLine a b in Line il (mk l) <#> indent 1 r
  where
    mk l = let w = maxLengthLine l in Group w $ Nest w l

block :: DocLength w => Doc' w -> Doc' w -> Doc' w -> Doc' w
block l r x = l <#> indent 1 x <#> r

encl :: DocLength w => Doc' w -> Doc' w -> Doc' w -> Doc' w
encl l r x = group $ l <-> x <> r

instance Enum w => Enum (Sum w) where
  toEnum = Sum . toEnum
  fromEnum (Sum w) = fromEnum w
  succ (Sum w) = Sum $ succ w
  pred (Sum w) = Sum $ pred w

flatten :: Doc' w -> Builder
flatten =
  foldDoc
    ( \i l ->
        string8 (replicate (fromEnum i) '\t')
          <> foldLine byteString (const . byteString) id id (<>) l
    )
    (\a b -> a <> "\n" <> b)

-- | Add max line length data to the doc
preprocess :: DocLength w => Doc' v -> (Any, Doc' w)
preprocess =
  foldDoc
    ( \i ->
        fmap (Line i)
          . foldLine
            (pure . Token)
            (\s d -> (Any True, Alt s $ snd $ preprocess d))
            (\(b, l) -> (b, if getAny b then Group (maxLengthLine l) l else l))
            (\(b, l) -> (b, if getAny b then Nest (maxLengthLine l) l else l))
            (liftA2 $ \a b -> Concat (maxLengthLine a <> maxLengthLine b) a b)
    )
    $ liftA2 (<#>)

breakLine :: DocLength w => Word -> Line w -> (Any, Doc' w)
breakLine i l = case l of
  Concat _ a b -> (<>) <$> breakLine i a <*> breakLine i b
  Token s -> pure $ Line i $ Token s
  Alt _ d -> (Any True, d)
  Group _ l -> (Any True, Line i l)
  Nest _ l -> let (b, d) = breakLine i l in (b, if getAny b then nest d else d)

breakDoc :: DocLength w => w -> Doc' w -> Doc' w
breakDoc w d = case d of
  Line i l | w < linelength i l && toEnum (4*(fromEnum i)) < w ->
    let (Any b, d) = breakLine i l in if b then breakDoc w d else d
  Lines w' a b | w < w' -> breakDoc w a <#> breakDoc w b
  _ -> d

layout :: Maybe Word -> Doc' w -> LB.ByteString
layout w d =
  toLazyByteString $
    maybe
      (flatten d)
      ( \w ->
          let (Any b, d') = preprocess d
           in flatten $ if b && Sum w < maxLengthDoc d' then breakDoc (Sum w) d' else d'
      )
      w
