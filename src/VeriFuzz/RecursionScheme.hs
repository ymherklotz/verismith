{-|
Module      : VeriFuzz.RecursionScheme
Description : Recursion scheme implementation for the AST.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Recursion scheme implementation for the AST.
-}

module VeriFuzz.RecursionScheme
    ( Term(..)
    , Attr(..)
    , Algebra
    , bottomUp
    , topDown
    , cata
    , ana
    , para
    , apo
    )
where

import           Control.Arrow ((&&&), (>>>), (|||))
import           Data.Function ((&))

newtype Term f = In { out :: f (Term f) }

data Attr f a = Attr
              { attribute :: a
              , hole      :: f (Attr f a)
              }

type Algebra f a = f a -> a

type RAlgebra f a = f (Term f, a) -> a

type RAlgebra' f a = Term f -> f a -> a

type CVAlgebra f a = f (Attr f a) -> a

type Coalgebra f a = a -> f a

type RCoalgebra f a = a -> f (Either (Term f) a)

bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp fn = cata $ In >>> fn

topDown :: Functor a => (Term a -> Term a) -> Term a -> Term a
topDown fn = ana $ fn >>> out

cata :: Functor f => Algebra f a -> Term f -> a
cata fn = para' $ const fn

ana :: Functor f => Coalgebra f a -> a -> Term f
ana fn =
    fn
    >>> fmap (ana fn)
    >>> In

para :: Functor f => RAlgebra f a -> Term f -> a
para ralg =
    out
    >>> fmap (id &&& para ralg)
    >>> ralg

para' :: Functor f => RAlgebra' f a -> Term f -> a
para' ralg t = out t & fmap (para' ralg) & ralg t

apo :: Functor f => RCoalgebra f a -> a -> Term f
apo rcoalg =
    rcoalg
    >>> fmap (id ||| apo rcoalg)
    >>> In

histo :: Functor f => CVAlgebra f a -> Term f -> a
histo cv =
    out
    >>> fmap worker
    >>> cv
    where
        worker t = undefined
