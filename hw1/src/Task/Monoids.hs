module Task.Monoids ( maybeConcat
                    , eitherConcat
                    , NonEmpty (..)
                    , Identity (..)
                    , Name (..)
                    , Arrow (..)
                    ) where

import Data.Semigroup

maybeConcat :: (Monoid a) => [Maybe a] -> a
maybeConcat = foldr helper mempty
  where
    helper (Just x) y = x `mappend` y
    helper Nothing  y = y

eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat = foldr helper (mempty, mempty)
  where
    helper (Left  x) (l, r) = (x `mappend` l, r)
    helper (Right x) (l, r) = (l, x `mappend` r)


data NonEmpty a = a :| [a]

instance Semigroup (NonEmpty a) where
  (x :| xs) <> (y :| ys) = x :| (xs ++ y : ys)

newtype Identity a = Identity { runIdentity :: a }

instance Semigroup a => Semigroup (Identity a) where
  (Identity l) <> (Identity r) = Identity $ l <> r

instance Monoid a => Monoid (Identity a) where
 mempty = Identity mempty

 (Identity l) `mappend` (Identity r) = Identity $ l `mappend` r


newtype Name = Name String

instance Semigroup Name where
 (Name "") <> (Name r)  = Name r
 (Name l)  <> (Name "") = Name l
 (Name l)  <> (Name r)  = Name $ l ++ "." ++ r

instance Monoid Name where
 mempty = Name ""
 l `mappend` r  = l <> r

newtype Arrow a b = Arrow { getArrow :: a -> b }

instance Semigroup b => Semigroup (Arrow a b) where
  (Arrow l) <> (Arrow r) = Arrow $ \x -> l x <> r x

instance Monoid b => Monoid (Arrow a b) where
  mempty = Arrow $ const mempty
  (Arrow l) `mappend` (Arrow r) = Arrow $ \x -> l x `mappend` r x

