module ParserSpec where

import Task.Parser
import Test.Hspec
import Control.Applicative ((<|>))

spec :: Spec
spec = do
  it "pass" $ do
    runParser (pass "pass") "" `shouldBe` Right ("", "pass")
    runParser (pass "pass") "anything" `shouldBe` Right ("anything", "pass")

  it "reject" $ do
    runParser (reject :: Monstupar Char Char) "anything" `shouldBe` Left Rejected

  it "eof" $ do
    runParser eof "" `shouldBe` Right ("", ())
    runParser eof "anything" `shouldBe` Left Rejected

  it "same" $ do
    runParser (same 'x') "x  " `shouldBe` Right ("  ", 'x')
    runParser (same 'x') "y  " `shouldBe` Left Rejected
    runParser (same 'x') ""    `shouldBe` Left EOF

  it "many" $ do
    runParser (many $ same 'x') ""    `shouldBe` Right ("", "")
    runParser (many $ same 'x') "x"   `shouldBe` Right ("", "x")
    runParser (many $ same 'x') "xx"  `shouldBe` Right ("", "xx")
    runParser (many $ same 'x') "xxy" `shouldBe` Right ("y", "xx")

  it "many1" $ do
    runParser (many1 $ same 'x') "x"    `shouldBe` Right ("", "x")
    runParser (many1 $ same 'x') "xxy"  `shouldBe` Right ("y", "xx")
    runParser (many1 $ same 'x') ""     `shouldBe` Left EOF
    runParser (many1 $ same 'x') "y"    `shouldBe` Left Rejected

  it "match" $ do
    runParser (match "xyz") "xyzw" `shouldBe` Right ("w", "xyz")
    runParser (match "xyz") "xyw"  `shouldBe` Left Rejected
    runParser (match "xyz") "xy"   `shouldBe` Left EOF

  it "<|>" $ do
    runParser (same 'x' <|> same 'y') "x" `shouldBe` Right ("", 'x')
    runParser (same 'x' <|> same 'y') "y" `shouldBe` Right ("", 'y')
    runParser (same 'x' <|> same 'y') "z" `shouldBe` Left Rejected
    runParser (same 'x' <|> same 'y') ""  `shouldBe` Left EOF
    runParser (same 'x' <|> reject)   ""  `shouldBe` Left EOF
    runParser (reject   <|> same 'x') ""  `shouldBe` Left EOF
