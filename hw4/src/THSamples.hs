{-# LANGUAGE TemplateHaskell #-}
module THSamples () where

import TH

data Foo = B Bool | I Int

deriveTextShow ''Foo
