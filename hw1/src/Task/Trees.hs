module Task.Trees where

import Data.Semigroup

data Tree a = Leaf | Node a (Tree a) (Tree a)

empty :: Tree a -> Bool
empty Leaf   = True
empty Node{} = False

count :: Tree a -> Int
count Leaf         = 0
count (Node _ l r) = count l + count r + 1

contains :: Ord a => a -> Tree a -> Bool
contains _ Leaf = False
contains x (Node y l r) | x == y    = True
                        | x < y     = contains x l
                        | otherwise = contains x r

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x (Node y l r) | x == y    = Node x l r
                      | x < y     = Node y (insert x l) r
                      | otherwise = Node y l (insert x r)

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert Leaf

instance Foldable Tree where
  foldMap _ Leaf = mempty
  foldMap f (Node x l r) = foldMap f l `mappend` f x `mappend` foldMap f r

  foldr _ z Leaf = z
  foldr f z (Node x l r) = foldr f (f x (foldr f z r)) l

instance Ord a => Semigroup (Tree a) where
  l <> r = foldr insert l r

instance Ord a => Monoid (Tree a) where
  mempty = Leaf
  l `mappend` r = l <> r

