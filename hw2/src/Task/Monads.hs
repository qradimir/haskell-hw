{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Task.Monads
     ( Monad (..)
     , MonadFish (..)
     , MonadJoin (..)
     ) where

class Monad m where
    return     :: a -> m a
    (>>=)      :: m a -> (a -> m b) -> m b

class MonadFish m where
    returnFish :: a -> m a
    (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

class MonadJoin m where
    returnJoin :: a -> m a
    join       :: m (m a) -> m a
