{-|
Module      : VeriFuzz.Result
Description : Result monadic type.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Result monadic type. This is nearly equivalent to the transformers 'Error' type,
but to have more control this is reimplemented with the instances that are
needed in "VeriFuzz".
-}

{-# LANGUAGE ScopedTypeVariables #-}

module VeriFuzz.Result
    ( Result(..)
    , ResultT(..)
    , (<?>)
    , annotate
    )
where

import           Control.Monad.IO.Class
import           Shelly                 (RunFailed (..), Sh, catch_sh)
import           Shelly.Lifted          (MonadSh, liftSh)

-- | Result type which is equivalent to 'Either' or 'Error'. This is
-- reimplemented so that there is full control over the 'Monad' definition and
-- definition of a 'Monad' transformer 'ResultT'.
data Result a b = Fail a
                | Pass b
                deriving (Eq, Show)

instance Semigroup (Result a b) where
    Fail _ <> b = b
    a <> _ = a

instance (Monoid b) => Monoid (Result a b) where
    mempty = Pass mempty

instance Functor (Result a) where
    fmap f (Pass a) = Pass $ f a
    fmap _ (Fail b) = Fail b

instance Applicative (Result a) where
    pure = Pass
    Fail e <*> _ = Fail e
    Pass f <*> r = fmap f r

instance Monad (Result a) where
    Pass a >>= f = f a
    Fail b >>= _ = Fail b

-- | The transformer for the 'Result' type. This
newtype ResultT a m b = ResultT { runResultT :: m (Result a b) }

instance Functor f => Functor (ResultT a f) where
    fmap f = ResultT . fmap (fmap f) . runResultT

instance Monad m => Applicative (ResultT a m) where
    pure = ResultT . pure . pure
    f <*> a = ResultT $ do
        f' <- runResultT f
        case f' of
            Fail e -> return (Fail e)
            Pass k -> do
                a' <- runResultT a
                case a' of
                    Fail e -> return (Fail e)
                    Pass v -> return (Pass $ k v)

instance Monad m => Monad (ResultT a m) where
    a >>= b = ResultT $ do
        m <- runResultT a
        case m of
            Fail e -> return (Fail e)
            Pass p -> runResultT (b p)

instance (MonadSh m, Monoid a) => MonadSh (ResultT a m) where
    liftSh s =
        ResultT
        . liftSh
        . catch_sh (Pass <$> s)
        $ (const (Fail <$> return mempty) :: RunFailed -> Sh (Result a b))

instance (MonadIO m) => MonadIO (ResultT a m) where
    liftIO s = ResultT $ Pass <$> liftIO s

infix 0 <?>

(<?>) :: (Monad m, Monoid a) => ResultT a m b -> a -> ResultT a m b
m <?> b = ResultT $ do
    a <- runResultT m
    case a of
        Pass a' -> return $ Pass a'
        Fail a' -> return . Fail $ a' <> b

annotate :: (Monad m, Monoid a) => a -> ResultT a m b -> ResultT a m b
annotate = flip (<?>)
