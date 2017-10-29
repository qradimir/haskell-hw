module Task.Identity where

newtype Identity a = Id { runIdentity :: a }

instance Functor Identity where
  fmap f (Id x) = Id $ f x

instance Applicative Identity where
  pure = Id
  (Id f) <*> (Id x) = Id $ f x

instance Foldable Identity where
  foldMap f (Id x) = f x

instance Traversable Identity where
  traverse f (Id x) = pure Id <*> f x
