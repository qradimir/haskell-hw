{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module Task.Monad where

import           Prelude     (Functor, const, id)
import           Task.Monads

--        LAWS
--        1. m >>= return    ≡ m
--        2. return a >>= f  ≡ f a
--        3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)


instance Monad m => MonadFish m where
  returnFish = return

  f >=> g = \x -> f x >>= g

  -- f >=> returnFish          == f
  -- f x >>= returnFish        == f x
  -- f x >>= return            == f x
  -- f x                       == f x

instance Monad m => MonadJoin m where
  returnJoin = return

  join m = m >>= id

  -- join . returnJoin   == id
  -- join (returnJoin x) == x
  -- return x >>= id     == x
  -- id x                == x
  -- x                   == x
