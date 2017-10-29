{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Task.MonadFish where

import           Prelude (id, const)
import           Task.Monads

--        LAWS
--        1. f >=> returnFish ≡ f
--        2. returnFish >=> f ≡ f
--        3. (f >=> g) >=> h  ≡ f >=> (g >=> h)

instance MonadFish m => Monad m where
 return = returnFish

 m >>= f = (const m >=> f) ()

instance MonadFish m => MonadJoin m where
 returnJoin = returnFish

 join m = (const m >=> id) ()
