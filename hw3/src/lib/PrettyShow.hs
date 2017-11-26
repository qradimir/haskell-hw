module PrettyShow ( PrettyShow(..)
                  , prettyShowWithParens
                  ) where

import Structure
import Evaluator
import Interpreter

class PrettyShow a where
  prettyShow :: a -> String

prettyShowWithParens :: PrettyShow a => Bool -> a -> String
prettyShowWithParens True  x = "(" ++ prettyShow x ++ ")"
prettyShowWithParens False x = prettyShow x

instance PrettyShow Expr where

  prettyShow (Lit int) = show int
  prettyShow (Var ref) = ref
  prettyShow (Add e1 e2) = prettyShowAdd e1 ++ " + " ++ prettyShowAdd e2
  prettyShow (Sub e1 e2) = prettyShowAdd e1 ++ " - " ++ prettyShowSub e2
  prettyShow (Mul e1 e2) = prettyShowMul e1 ++ " * " ++ prettyShowAdd e2
  prettyShow (Div e1 e2) = prettyShowMul e1 ++ " / " ++ prettyShowDiv e2
  prettyShow (Let r d e) = "let " ++ r ++ " = " ++ prettyShow d ++ " in " ++ prettyShow e


parensInAdd :: Expr -> Bool
parensInAdd Let{} = True
parensInAdd _     = False

parensInSub :: Expr -> Bool
parensInSub Add{} = True
parensInSub Let{} = True
parensInSub _     = False

parensInMul :: Expr -> Bool
parensInMul Add{} = True
parensInMul Sub{} = True
parensInMul Let{} = True
parensInMul _     = False

parensInDiv :: Expr -> Bool
parensInDiv Add{} = True
parensInDiv Sub{} = True
parensInDiv Div{} = True
parensInDiv Let{} = True
parensInDiv _     = False

prettyShowAdd :: Expr -> String
prettyShowAdd e = prettyShowWithParens (parensInAdd e) e

prettyShowSub :: Expr -> String
prettyShowSub e = prettyShowWithParens (parensInSub e) e

prettyShowMul :: Expr -> String
prettyShowMul e = prettyShowWithParens (parensInMul e) e

prettyShowDiv :: Expr -> String
prettyShowDiv e = prettyShowWithParens (parensInDiv e) e


instance PrettyShow Statement where
  prettyShow (NewVarStatement ref e)  = "mut " ++ ref ++ " = " ++ prettyShow e
  prettyShow (UpdVarStatement ref e)  = ref ++ " = " ++ prettyShow e
  prettyShow (PrintStatement e)       = "< " ++ prettyShow e
  prettyShow (ReadStatement ref)      = "> " ++ ref
  prettyShow (ForStatement ref b e _) = "for " ++ ref ++ " in " ++ prettyShow b ++ " .. " ++ prettyShow e ++ " ..."
  prettyShow (IfStatement e _)        = "if " ++ prettyShow e ++ "then ..."
  prettyShow BreakStatement           = "break"

instance PrettyShow EvalError where
  prettyShow DivizionByZero          = "Divizion by zero"
  prettyShow (NoVarBindingFound ref) = "Variable " ++ ref ++ " not found"

instance PrettyShow InterpreterError where
  prettyShow (EvaluationError e)    = prettyShow e
  prettyShow (VarRedefError ref)    = "Redefinition of variable " ++ ref
  prettyShow (NoVarUpdateError ref) = "Variable " ++ ref ++ " not defined"

instance PrettyShow InterpreterErrorInfo where
  prettyShow (ErrInfo stmt er) = "Error occured in statement '" ++ prettyShow stmt ++ "':\n\t" ++ prettyShow er
