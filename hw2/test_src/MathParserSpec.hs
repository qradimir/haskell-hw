module MathParserSpec where

import Task.MathParser
import Task.Parser
import Test.Hspec


spec :: Spec
spec = do
  it "parse" $ do
    runParser exprParser "let x = 2" `shouldBe` Right ("", Expr (Name "x") [Const 2])
    runParser exprParser "let x=2" `shouldBe` Right ("", Expr (Name "x") [Const 2])
    runParser exprParser "  let   x  =  2" `shouldBe` Right ("", Expr (Name "x") [Const 2])
    runParser exprParser "let y = 2 + x" `shouldBe` Right ("", Expr (Name "y") [Const 2, Ref $ Name "x"])
    runParser exprParser "let y = 2 +" `shouldBe` Right (" +", Expr (Name "y") [Const 2])

    runParser exprParser "let y = " `shouldBe` Left EOF
    runParser exprParser "lety = 2" `shouldBe` Left Rejected
    runParser exprParser "let y 2" `shouldBe` Left Rejected
    runParser exprParser "let y = x2" `shouldBe` Left Rejected

  it "constant-fold" $ do
    fold [Expr (Name "x") [Const 2, Const 2]] `shouldBe` [Expr (Name "x") [Const 4]]
    fold [
       Expr (Name "x") [Const 2],
       Expr (Name "y") [Const 2, Ref (Name "x")]
      ] `shouldBe` [
        Expr (Name "x") [Const 2],
        Expr (Name "y") [Const 4]
      ]
    fold [
        Expr (Name "x") [Const 2, Const 4],
        Expr (Name "y") [Const 2, Ref (Name "x")],
        Expr (Name "z") [Const 4, Ref (Name "x"), Const 4, Ref (Name "y")]
      ] `shouldBe` [
        Expr (Name "x") [Const 6],
        Expr (Name "y") [Const 8],
        Expr (Name "z") [Const 22]
      ]
