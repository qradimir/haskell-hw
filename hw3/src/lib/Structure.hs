module Structure
     ( Ref
     , Dictionary
     , Expr (..)
     , Statement (..)
     , Program
     ) where

import qualified Data.Map.Lazy as M (Map)


type Ref = String
type Dictionary = M.Map Ref Int

data Expr = Lit Int
          | Var Ref
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Let Ref Expr Expr
  deriving (Eq, Show)

data Statement = NewVarStatement Ref Expr
               | UpdVarStatement Ref Expr
               | PrintStatement  Expr
               | ReadStatement   Ref
               | ForStatement    Ref Expr Expr [Statement]
               | IfStatement     Expr [Statement]
               | BreakStatement
  deriving (Eq, Show)

type Program = [Statement]
