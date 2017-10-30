{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module Task.MonadFish where

import           Prelude     (const, id)
import           Task.Monads

--        LAWS
--        1. f >=> returnFish ≡ f
--        2. returnFish >=> f ≡ f
--        3. (f >=> g) >=> h  ≡ f >=> (g >=> h)

instance MonadFish m => Monad m where
 return = returnFish

 m >>= f = (const m >=> f) ()


 -- m >>= return                ==  m
 -- (const m >=> returnFish) () ==  m
 -- const m ()                  ==  m
 -- m                           ==  m


instance MonadFish m => MonadJoin m where
 returnJoin = returnFish

 join m = (const m >=> id) ()

 -- join . returnJoin                == id
 -- (join . returnJoin) m            == m
 -- join (returnJoin m)              == m
 -- (const (returnJoin m) >=> id) () == m
 -- const (returnJoin m) >=> id      == const m
 -- (const . returnJoin) m >=> id    == const m
 --


 -- join . fmap returnJoin        == id
 -- (join . fmap returnJoin) m    == m
 -- join (fmap returnJoin m)      == m
 -- (const (fmap returnJoin m) >=> id) () == m
