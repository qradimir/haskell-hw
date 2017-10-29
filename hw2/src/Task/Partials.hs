{-# LANGUAGE TypeOperators #-}

module Task.Partials
       ( partial
       , total
       , apply
       , applyOrElse
       , withDefault
       , isDefinedAt
       , orElse
       ) where

import           Data.Maybe (isJust, fromMaybe)
import           Control.Applicative ((<|>))
import           Control.Category (Category, id, (.))
import           Control.Monad ((>=>))
import           Prelude hiding (id, (.))

data a ~> b
    = Partial   (a -> Maybe b) -- a partial function
    | Defaulted (a ~> b) b     -- a partial function with a default value


partial :: (a -> Maybe b) -> a ~> b
partial = Partial

total :: (a -> b) -> a ~> b
total f = Partial $ Just . f

apply       :: (a ~> b) -> a -> Maybe b
apply (Partial f)     x = f x
apply (Defaulted f d) x = apply f x <|> Just d

applyOrElse :: (a ~> b) -> a -> b -> b
applyOrElse f a b = fromMaybe b $ apply f a

withDefault :: (a ~> b) -> b -> (a ~> b)
withDefault = Defaulted

isDefinedAt :: (a ~> b) -> a -> Bool
isDefinedAt f a = isJust $ apply f a

orElse :: (a ~> b) -> (a ~> b) -> a ~> b
orElse f g = Partial $ \x -> apply g x <|> apply f x

instance Category (~>) where
  id = total id

  g . f = Partial $ apply f >=> apply g
