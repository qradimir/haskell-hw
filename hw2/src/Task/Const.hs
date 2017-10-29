module Task.Const
     ( Const(..)
     ) where


newtype Const a b = Const { getConst :: a }

instance Functor (Const a) where
  fmap _ (Const x) = Const x

instance Monoid a => Applicative (Const a) where
  pure _ = Const mempty

  (Const f) <*> (Const x) = Const $ f `mappend` x

instance Foldable (Const a) where
  foldMap _ _ = mempty

instance Traversable (Const a) where
  traverse _ (Const x) = pure $ Const x
