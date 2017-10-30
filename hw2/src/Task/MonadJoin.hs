{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module Task.MonadJoin where

import           Prelude     (Functor (..), const, id, ($), (.))
import           Task.Monads

--        LAWS
--        1. join . returnJoin      ≡ id
--        2. join . fmap returnJoin ≡ id
--        3. join . fmap join       ≡ join . join
--        4* join . fmap (fmap f)   ≡ fmap f . join

instance (Functor m, MonadJoin m) => Monad m where
  return = returnJoin

  m >>= f = join $ fmap f m

  --  m >>= return                == m
  --  join $ fmap return       m  == m
  --  (join . fmap returnJoin) m  == m
  --  id m                        == m
  --  m                           == m

instance (Functor m, MonadJoin m) => MonadFish m where
  returnFish = returnJoin

  f >=> g = join . fmap g . f

  -- f >=> returnFish             == f
  -- join . fmap returnJoin . f   == f
  -- (join . fmap returnJoin) . f == f
  -- id . f                       == f
  -- f                            == f
