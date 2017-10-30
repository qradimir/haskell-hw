module ExpressionsSpec where


import           Task.Expressions
import           Test.Hspec
import           Prelude hiding (div)


cm1 :: Expr
cm1 = constant (-1)
c0 :: Expr
c0 = constant 0
c1 :: Expr
c1 = constant 1
c2 :: Expr
c2 = constant 2
c3 :: Expr
c3 = constant 3
c4 :: Expr
c4 = constant 4


spec :: Spec
spec = do
  it "constant" $ do
    eval c3 `shouldBe` Right 3
    eval c0 `shouldBe` Right 0

  it "add" $ do
    eval (c2 `add` c3) `shouldBe` Right 5
    eval (c2 `add` c0) `shouldBe` Right 2

  it "sub" $ do
    eval (c3 `sub` c1) `shouldBe` Right 2
    eval (c1 `sub` c3) `shouldBe` Right (-2)

  it "mul" $ do
    eval (c3 `mul` c2) `shouldBe` Right 6
    eval (c3 `mul` c0) `shouldBe` Right 0

  it "div" $ do
    eval (c4 `div` c2) `shouldBe` Right 2
    eval (c4 `div` c0) `shouldBe` Left DivByZero
    eval (c0 `div` c2) `shouldBe` Right 0

  it "pow" $ do
    eval (c3 `pow` c2) `shouldBe` Right 9
    eval (c3 `pow` c0) `shouldBe` Right 1
    eval (c3 `pow` cm1) `shouldBe` Left PowByNeg
