{-# LANGUAGE MultiParamTypeClasses #-}
module Renew
     ( Renew(..)
     , MonoidAction(..)
     ) where

import Control.Comonad

data Renew s e a = Renew (e -> a) s

class Monoid e => MonoidAction s e where
    act :: s -> e -> s

instance Functor (Renew s e) where
  fmap ab (Renew ea s) = Renew (ab.ea) s

instance MonoidAction s e => Comonad (Renew s e) where
  extract (Renew ea _) = ea mempty
  extend f (Renew ea s) = Renew (\e1 -> f $ Renew (\e2 -> ea $ e1 `mappend` e2) (s `act` e1)) s
  duplicate (Renew ea s) = Renew (\e1 -> Renew (\e2 -> ea $ e1 `mappend` e2) (s `act` e1)) s
