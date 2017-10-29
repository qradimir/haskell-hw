module Task.Either
    ( Either (..)
    ) where

import Prelude hiding (Either (..))

data Either a b = Left a | Right b

instance Functor (Either a) where
  fmap _ (Left x) = Left x
  fmap f (Right x) = Right $ f x

instance Applicative (Either a) where
  pure = Right

  (Left f)  <*> _         = Left f
  _         <*> (Left x)  = Left x
  (Right f) <*> (Right x) = Right $ f x

instance Foldable (Either a) where
  foldMap _ (Left _)  = mempty
  foldMap f (Right x) = f x

instance Traversable (Either a) where
  traverse _ (Left x)  = pure $ Left x
  traverse f (Right x) = pure Right <*> f x
