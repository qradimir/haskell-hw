{-# LANGUAGE RankNTypes #-}
module Iso
     ( Iso
     , iso
     , getTo
     , getFrom
     , from
     , fsTreeIso
     ) where

import Data.Profunctor
import Data.Tagged
import Data.Functor.Const
import Data.Functor.Identity
import Data.Tree
import FS

type Iso b a = forall p f. (Profunctor p, Functor f) => p a (f a) -> p b (f b)

iso :: (b -> a) -> (a -> b) -> Iso b a
iso to' from' = dimap to' (fmap from')

getTo :: Iso b a -> b -> a
getTo iso' b = getConst $ iso' Const b

getFrom :: Iso b a -> a -> b
getFrom iso' a = runIdentity . untag $ iso' (Tagged (Identity a))

from :: Iso b a -> Iso a b
from iso' = iso (getFrom iso') (getTo iso')

fsTreeIso :: Iso FS (Tree FilePath)
fsTreeIso = iso fsToTree treeToFS
  where
    treeToFS :: Tree FilePath -> FS
    treeToFS t = case subForest t of
                   [] -> File $ rootLabel t
                   sf -> Dir (rootLabel t) (map treeToFS sf)

    fsToTree :: FS -> Tree FilePath
    fsToTree (File n)   = Node {rootLabel = n, subForest = []}
    fsToTree (Dir n fs) = Node {rootLabel = n, subForest = map fsToTree fs}
