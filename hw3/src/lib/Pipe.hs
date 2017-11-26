{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pipe where

import Control.Monad.IO.Class
import Control.Monad.State


class Monad m => MonadPipe v m | m -> v where
  readValue :: m v
  writeValue :: v -> m ()


newtype IOPipe v a = MkPipe { runIO :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Read v, Show v) => MonadPipe v (IOPipe v) where
  readValue  = MkPipe readLn
  writeValue = MkPipe . print


newtype ListPipe v a = MkListPipe { runList :: State ([v], [v]) a }
  deriving (Functor, Applicative, Monad)

instance MonadPipe v (ListPipe v) where
  readValue = MkListPipe $ do
      (reads', writes') <- get
      case reads' of
        [] -> fail "Pipe closed"
        (fst':rest') -> do
            put (rest',writes')
            return fst'

  writeValue v = MkListPipe $ do
      (reads', writes') <- get
      put (reads', v:writes')
