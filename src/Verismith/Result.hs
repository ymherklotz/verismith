{-|
Module      : Verismith.Result
Description : Result monadic type.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : yann [at] yannherklotz [dot] com
Stability   : experimental
Portability : POSIX

Result monadic type. This is nearly equivalent to the transformers 'Error' type,
but to have more control this is reimplemented with the instances that are
needed in "Verismith".
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Verismith.Result
    ( Result(..)
    , ResultT(..)
    , justPass
    , justFail
    , (<?>)
    , annotate
    )
where

import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Control
import           Data.Bifunctor              (Bifunctor (..))
import           Shelly                      (RunFailed (..), Sh, catch_sh)
import           Shelly.Lifted               (MonadSh, liftSh)

-- | Result type which is equivalent to 'Either' or 'Error'. This is
-- reimplemented so that there is full control over the 'Monad' definition and
-- definition of a 'Monad' transformer 'ResultT'.
data Result a b = Fail a
                | Pass b
                deriving (Eq, Show)

justPass :: Result a b -> Maybe b
justPass (Fail _) = Nothing
justPass (Pass a) = Just a

justFail :: Result a b -> Maybe a
justFail (Pass _) = Nothing
justFail (Fail a) = Just a

instance Semigroup (Result a b) where
    Pass _ <> a = a
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

instance MonadBase (Result a) (Result a) where
    liftBase = id

instance Bifunctor Result where
    bimap a _ (Fail c) = Fail $ a c
    bimap _ b (Pass c) = Pass $ b c

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

instance MonadIO m => MonadIO (ResultT a m) where
    liftIO s = ResultT $ Pass <$> liftIO s

instance MonadBase b m => MonadBase b (ResultT a m) where
    liftBase = liftBaseDefault

instance MonadTrans (ResultT e) where
    lift m = ResultT $ Pass <$> m

instance MonadTransControl (ResultT a) where
    type StT (ResultT a) b = Result a b
    liftWith f = ResultT $ return <$> f runResultT
    restoreT = ResultT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance MonadBaseControl IO m => MonadBaseControl IO (ResultT a m) where
    type StM (ResultT a m) b = ComposeSt (ResultT a) m b
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM
    {-# INLINABLE liftBaseWith #-}
    {-# INLINABLE restoreM #-}

infix 0 <?>

(<?>) :: (Monad m, Monoid a) => ResultT a m b -> a -> ResultT a m b
m <?> b = ResultT $ do
    a <- runResultT m
    case a of
        Pass a' -> return $ Pass a'
        Fail a' -> return . Fail $ a' <> b

annotate :: (Monad m, Monoid a) => a -> ResultT a m b -> ResultT a m b
annotate = flip (<?>)
