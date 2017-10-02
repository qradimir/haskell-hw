module SplitSpec where

import           Task.Split
import           Test.Hspec

spec :: Spec
spec = do
    it "splitOn" $ do
        splitOn '/' "path/to/file"  `shouldBe` ["path", "to", "file"]
        splitOn '/' "path/to/file/" `shouldBe` ["path", "to", "file", ""]
        splitOn '/' "/"             `shouldBe` ["", ""]
        splitOn '/' ""              `shouldBe` [""]

    it "joinWith" $ do
        joinWith '/' ["path", "to", "file"]     `shouldBe` "path/to/file"
        joinWith '/' ["path", "to", "file", ""] `shouldBe` "path/to/file/"
        joinWith '/' ["", ""]                   `shouldBe` "/"
        joinWith '/' [""]                       `shouldBe` ""

