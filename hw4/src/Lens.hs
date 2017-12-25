{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}

module Lens
     ( Lens
     , Lens'
     , view
     , over
     , set
     , (.~)
     , (^.)
     , (%~)
     , _1
     , _2
     , lens
     , choosing
     , (<%~)
     , (<<%~)
     ) where

import Data.Functor.Identity
import Data.Functor.Const
import Control.Arrow ((&&&))

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

view :: Lens s t a b -> s -> a
view l = getConst . l Const

over :: Lens s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

set  :: Lens s t a b -> b -> s -> t
set l b = over l (const b)

infixl 1 .~
(.~) :: Lens' s a -> a -> s -> s
(.~) = set

infixl 8 ^.
(^.) :: s -> Lens' s a -> a
(^.) s l = view l s

infixr 4 %~
(%~) :: Lens' s a -> (a -> a) -> s -> s
(%~) = over


-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 f (t, x) = (, x) <$> f t

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 f (x, t) = (x, ) <$> f t

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get' set' f s = set' s <$> f (get' s)


-- Объединить две линзы в одну, которая работает с Either.
choosing :: Lens s1 t1 a b
         -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 = lens (either (view l1) (view l2)) (either (\s b -> Left (set l1 b s)) (\s b -> Right (set l2 b s)))

-- Изменить цель линзы и вернуть новый результат. Постарайтесь
-- реализовать без анонимных функций и определений своих функций
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f = l (f &&& f)

-- Изменить цель линзы, но вернуть старый результат.
(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f = l (id &&& f)
