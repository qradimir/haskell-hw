{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Task.MonadJoin where

import           Prelude (id, const, Functor(..), (.), ($))
import           Task.Monads

--        LAWS
--        1. join . pure            ≡ id
--        2. join . fmap returnJoin ≡ id
--        3. join . fmap join       ≡ join . join

instance (Functor m, MonadJoin m) => Monad m where
  return = returnJoin

  m >>= f = join $ fmap f m

instance (Functor m, MonadJoin m) => MonadFish m where
  returnFish = returnJoin

  f >=> g = join . fmap g . f
