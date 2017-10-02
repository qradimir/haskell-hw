module Task.Nats where

import Prelude hiding (even, mod, gcd)

data Nat = Z | S Nat

instance Eq Nat where
  Z     == Z     = True
  Z     == _     = False
  _     == Z     = False
  (S l) == (S r) = l == r

instance Ord Nat where
  compare Z     Z     = EQ
  compare Z     _     = LT
  compare _     Z     = GT
  compare (S l) (S r) = compare l r

instance Num Nat where
  x + Z = x
  x + S y = S $ x + y

  _ * Z = Z
  x * S y = x + x * y

  x     - Z     = x
  Z     - _     = Z
  (S x) - (S y) = x - y

  abs    x = x
  signum _ = 1

  fromInteger int | int <= 0  = Z
                  | otherwise = S . fromInteger $ int - 1

even :: Nat -> Bool
even Z     = True
even (S x) = not $ even x

div :: Nat -> Nat -> Nat
div _ Z = error "Division by zero"
div l r = helper l 0
  where
    helper :: Nat -> Nat -> Nat
    helper val res | val <  0  = helper (val + r) (res - 1)
                   | val >= r  = helper (val - r) (res + 1)
                   | otherwise = res

mod :: Nat -> Nat -> Nat
mod _ Z             = error "Division by zero"
mod l r | l <  0    = mod (l + r) r
        | l >= r    = mod (l - r) r
        | otherwise = l

gcd :: Nat -> Nat -> Nat
gcd a Z = a
gcd Z b = b
gcd a b = gcd b (mod a b)