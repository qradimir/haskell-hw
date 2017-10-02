module ListsSpec where

import           Task.Lists
import           Test.Hspec

spec :: Spec
spec = do
    it "removeAt" $ do
        removeAt 1 [1,2,3] `shouldBe` (Just 2, [1,3])
        removeAt 10 [1,2,3] `shouldBe` (Nothing, [1,2,3])
        removeAt 3 [1..5] `shouldBe` (Just 4, [1,2,3,5])
        removeAt 2 "abc" `shouldBe` (Just 'c', "ab")
        removeAt 0 "abc" `shouldBe` (Just 'a', "bc")
        removeAt (-2) "abc" `shouldBe` (Nothing, "abc")

    it "collectEvery" $ do
        collectEvery 3 [1..8] `shouldBe` ([1,2,4,5,7,8], [3,6])
        collectEvery 2 [1..8] `shouldBe` ([1,3,5,7], [2,4,6,8])
        collectEvery 1 [1..8] `shouldBe` ([], [1..8])
        collectEvery 9 [1..8] `shouldBe` ([1..8], [])
        collectEvery 0 [1..8] `shouldBe` ([1..8], [])

    it "stringSum" $ do
        stringSum "5 5 +5 -5" `shouldBe` 10
        stringSum "5\n\t 5 \t\n+5 \n\t\t\n-5" `shouldBe` 10

    it "mergeSort" $ do
        mergeSort [1..8]                `shouldBe` [1..8]
        mergeSort [8,7..1]              `shouldBe` [1..8]
        mergeSort ([1..4] ++ [4,3..1]) `shouldBe` [1,1,2,2,3,3,4,4]
        mergeSort [2, 1, 0, 3, 10, 5]   `shouldBe` [0, 1, 2, 3, 5, 10]

