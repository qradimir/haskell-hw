module Task.Expressions
       ( Expr (..)
       , ArithmeticError (..)
       , BinOperator (..)
       , eval
       ) where

data Expr
    = Constant Int
    | BinOperation BinOperator Expr Expr

data BinOperator = Add | Sub | Mul | Div | Pow

data ArithmeticError = DivByZero | PowByNeg

apply :: BinOperator -> Int -> Int -> Either ArithmeticError Int
apply Add l r = Right $ l + r
apply Sub l r = Right $ l - r
apply Mul l r = Right $ l * r
apply Div _ 0 = Left DivByZero
apply Div l r = Right $ l `div` r
apply Pow l r | r < 0     = Left PowByNeg
              | otherwise = Right $ l ^ r

eval :: Expr -> Either ArithmeticError Int
eval (Constant x) = Right x
eval (BinOperation o l r) = eval l >>= \lv -> eval r >>= apply o lv
