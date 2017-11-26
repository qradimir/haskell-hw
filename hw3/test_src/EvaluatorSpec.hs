module EvaluatorSpec where

import           Structure
import           Evaluator
import           Test.Hspec
import qualified Data.Map as M


spec :: Spec
spec = do
    it "lit" $
      evaluateExpr (Lit 10) M.empty `shouldBe` Right 10

    it "var" $ do
      evaluateExpr (Var "x") (M.fromList [("x", 10)]) `shouldBe` Right 10
      evaluateExpr (Var "x") M.empty `shouldBe` Left (NoVarBindingFound "x")

    it "add" $
      evaluateExpr (Lit 2 `Add` Lit 3) M.empty `shouldBe` Right 5

    it "sub" $
      evaluateExpr (Lit 2 `Sub` Lit 3) M.empty `shouldBe` Right (-1)

    it "mul" $
      evaluateExpr (Lit 2 `Mul` Lit 3) M.empty `shouldBe` Right 6

    it "div" $ do
      evaluateExpr (Lit 6 `Div` Lit 3) M.empty `shouldBe` Right 2
      evaluateExpr (Lit 5 `Div` Lit 0) M.empty `shouldBe` Left DivizionByZero

    it "Let" $ do
      evaluateExpr (Let "x" (Lit 2) (Var "x")) M.empty `shouldBe` Right 2
      evaluateExpr (Let "x" (Lit 2) (Var "x")) (M.fromList [("x", 3)]) `shouldBe` Right 2
      evaluateExpr (Let "x" (Var "x") (Var "x")) M.empty  `shouldBe` Left (NoVarBindingFound "x")
      evaluateExpr (Let "x" (Var "x") (Var "x")) (M.fromList [("x", 3)]) `shouldBe` Right 3
