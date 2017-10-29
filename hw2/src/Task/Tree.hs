module Task.Tree
    (
    ) where


data Tree a = Leaf | Node a (Tree a) (Tree a)

instance Functor Tree where
  fmap _ Leaf         = Leaf
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

instance Applicative Tree where
  pure x = let tree = Node x tree tree in tree

  (Node f fl fr) <*> (Node x xl xr) = Node (f x) (fl <*> xl) (fr <*> xr)
  _              <*> _              = Leaf

instance Foldable Tree where
  foldMap _ Leaf = mempty
  foldMap f (Node x l r) = foldMap f l `mappend` f x `mappend` foldMap f r

  foldr _ z Leaf = z
  foldr f z (Node x l r) = foldr f (f x (foldr f z r)) l

instance Traversable Tree where
  traverse _ Leaf         = pure Leaf
  traverse f (Node x l r) = pure Node <*> f x <*> traverse f l <*> traverse f r
