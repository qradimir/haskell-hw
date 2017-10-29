{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Task.Monad where

import           Prelude (id, const, Functor)
import           Task.Monads

--        LAWS
--        1. m >>= return    ≡ m
--        2. return a >>= f  ≡ f a
--        3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)


instance Monad m => MonadFish m where
  returnFish = return

  f >=> g = \x -> f x >>= g

instance Monad m => MonadJoin m where
  returnJoin = return

  join m = m >>= id
