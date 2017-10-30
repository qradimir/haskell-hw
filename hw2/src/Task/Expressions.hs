module Task.Expressions
       ( Expr
       , ArithmeticError (..)
       , constant
       , add
       , sub
       , mul
       , Task.Expressions.div
       , pow
       , eval
       ) where

import           Prelude
import qualified Prelude as P (div)

data Expr
    = Constant Int
    | BinOperation BinOperator Expr Expr

data BinOperator = Add | Sub | Mul | Div | Pow

data ArithmeticError = DivByZero | PowByNeg
  deriving (Show, Eq)

apply :: BinOperator -> Int -> Int -> Either ArithmeticError Int
apply Add l r = Right $ l + r
apply Sub l r = Right $ l - r
apply Mul l r = Right $ l * r
apply Div _ 0 = Left DivByZero
apply Div l r = Right $ l `P.div` r
apply Pow l r | r < 0     = Left PowByNeg
              | otherwise = Right $ l ^ r

eval :: Expr -> Either ArithmeticError Int
eval (Constant x)         = Right x
eval (BinOperation o l r) = eval l >>= \lv -> eval r >>= apply o lv


constant :: Int -> Expr
constant = Constant

add :: Expr -> Expr -> Expr
add = BinOperation Add

sub :: Expr -> Expr -> Expr
sub = BinOperation Sub

mul :: Expr -> Expr -> Expr
mul = BinOperation Mul

div :: Expr -> Expr -> Expr
div = BinOperation Div

pow :: Expr -> Expr -> Expr
pow = BinOperation Pow
