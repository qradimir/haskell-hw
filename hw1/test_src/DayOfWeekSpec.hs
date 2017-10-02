module DayOfWeekSpec where

import           Task.DayOfWeek
import           Test.Hspec

spec :: Spec
spec = do
    it "nextDay" $ do
        nextDay Mon `shouldBe` Tue
        nextDay Tue `shouldBe` Wed
        nextDay Wed `shouldBe` Thu
        nextDay Thu `shouldBe` Fri
        nextDay Fri `shouldBe` Sat
        nextDay Sat `shouldBe` Sun
        nextDay Sun `shouldBe` Mon

    it "isWeekend" $ do
        isWeekend Mon `shouldBe` False
        isWeekend Tue `shouldBe` False
        isWeekend Wed `shouldBe` False
        isWeekend Thu `shouldBe` False
        isWeekend Fri `shouldBe` False
        isWeekend Sat `shouldBe` True
        isWeekend Sun `shouldBe` True

    it "afterDays" $ do
        afterDays 1 Mon  `shouldBe` Tue
        afterDays 2 Mon  `shouldBe` Wed
        afterDays 5 Mon  `shouldBe` Sat
        afterDays 7 Mon  `shouldBe` Mon
        afterDays 70 Mon `shouldBe` Mon
        afterDays 75 Mon `shouldBe` Sat

    it "daysToParty" $ do
        daysToParty Mon `shouldBe` 4
        daysToParty Tue `shouldBe` 3
        daysToParty Wed `shouldBe` 2
        daysToParty Thu `shouldBe` 1
        daysToParty Fri `shouldBe` 0
        daysToParty Sat `shouldBe` 6
        daysToParty Sun `shouldBe` 5
