module Task.Pair
  ( Pair (..)
  ) where

data Pair a b = Pair a b

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Monoid a => Applicative (Pair a) where
  pure = Pair mempty

  (Pair fa fb) <*> (Pair xa xb) = Pair (fa `mappend` xa) (fb xb)

instance Foldable (Pair a) where
  foldMap f (Pair _ b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = pure (Pair a) <*> f b 
