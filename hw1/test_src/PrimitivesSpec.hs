module PrimitivesSpec where

import           Task.Primitives
import           Test.Hspec

spec :: Spec
spec = do
    it "order3" $ do
        order3 ("a", "b", "c") `shouldBe` ("a", "b", "c")
        order3 ("a", "c", "b") `shouldBe` ("a", "b", "c")
        order3 ("b", "a", "c") `shouldBe` ("a", "b", "c")
        order3 ("b", "c", "a") `shouldBe` ("a", "b", "c")
        order3 ("c", "a", "b") `shouldBe` ("a", "b", "c")
        order3 ("c", "b", "a") `shouldBe` ("a", "b", "c")

    it "highestBit" $ do
        highestBit 15 `shouldBe` (8, 3)
        highestBit 16 `shouldBe` (16, 4)
        highestBit 17 `shouldBe` (16, 4)

    it "smartReplicate" $ do
        smartReplicate [1]       `shouldBe` [1]
        smartReplicate [2]       `shouldBe` [2, 2]
        smartReplicate [1, 2, 3] `shouldBe` [1, 2, 2, 3, 3, 3]
        smartReplicate [0]       `shouldBe` []
        smartReplicate []        `shouldBe` []

    it "containts" $ do
        contains "a" [["a"], ["b"]] `shouldBe` [["a"]]
        contains "a" [["a", "b"]]   `shouldBe` [["a", "b"]]
        contains "a" [["b", "a"]]   `shouldBe` [["b", "a"]]
        contains "a" [["b", "c"]]   `shouldBe` []
