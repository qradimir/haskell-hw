module MathParserSpec where

import Task.MathParser
import Task.Parser
import Test.Hspec


spec :: Spec
spec = do
  it "ab-parse" $ do
    runParser abParser "ab"  `shouldBe` Right ("", ('a','b'))
    runParser abParser "a"   `shouldBe` Left EOF
    runParser abParser "abc" `shouldBe` Right ("c", ('a', 'b'))
    runParser abParser "ba"  `shouldBe` Left Rejected

  it "s-parse" $ do
    runParser sExprParser "a"        `shouldBe` Right ("", Atom . Ref . Name $ "a")
    runParser sExprParser "1"        `shouldBe` Right ("", Atom . Const $ 1)
    runParser sExprParser "(a 1)"    `shouldBe` Right ("", Comb [Atom . Ref . Name $ "a", Atom . Const $ 1])
    runParser sExprParser "()"       `shouldBe` Right ("", Comb [])
    runParser sExprParser "(1 () 2)" `shouldBe` Right ("", Comb [Atom . Const $ 1, Comb [], Atom . Const $ 2])

  it "let-parse" $ do
    runParser exprParser "let x = 2" `shouldBe` Right ("", Expr (Name "x") [Const 2])
    runParser exprParser "let x=2" `shouldBe` Right ("", Expr (Name "x") [Const 2])
    runParser exprParser "  let   x  =  2" `shouldBe` Right ("", Expr (Name "x") [Const 2])
    runParser exprParser "let y = 2 + x" `shouldBe` Right ("", Expr (Name "y") [Const 2, Ref $ Name "x"])
    runParser exprParser "let y = 2 +" `shouldBe` Right (" +", Expr (Name "y") [Const 2])
    runParser exprParser "let y = x2" `shouldBe` Right ("", Expr (Name "y") [Ref . Name $ "x2"])

    runParser exprParser "let y = " `shouldBe` Left EOF
    runParser exprParser "lety = 2" `shouldBe` Left Rejected
    runParser exprParser "let y 2" `shouldBe` Left Rejected

  it "let-constant-fold" $ do
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
