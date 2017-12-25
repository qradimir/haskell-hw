module Tree
     ( Tree (..)
     ) where

import Control.Comonad

data Tree a = Node a [Tree a]

instance Functor Tree where
  fmap f (Node a subForest) = Node (f a) (map (fmap f) subForest)

instance Comonad Tree where
  extract (Node a _) = a

  duplicate n@(Node _ subForest) = Node n (map duplicate subForest)
