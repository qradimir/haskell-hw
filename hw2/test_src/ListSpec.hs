module ListSpec where


import           Task.List
import           Test.Hspec
import qualified Data.List as L

spec :: Spec
spec = do
  it "bin" $ do
    bin 0 `shouldMatchList` [[]]
    bin 1 `shouldMatchList` [[0],[1]]
    bin 2 `shouldMatchList` [[0, 0], [0, 1], [1, 0], [1, 1]]
    bin 3 `shouldMatchList` [[0, 0, 0], [0, 0, 1], [0, 1, 0], [0, 1, 1], [1, 0, 0], [1, 0, 1], [1, 1, 0], [1, 1, 1]]

  it "comb" $ do
    combinations 4 0 `shouldMatchList` [[]]
    combinations 4 1 `shouldMatchList` [[1], [2], [3], [4]]
    combinations 4 2 `shouldMatchList` [[1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]]
    combinations 4 3 `shouldMatchList` [[1, 2, 3], [1, 2, 4], [1, 3, 4], [2, 3, 4]]
    combinations 4 4 `shouldMatchList` [[1, 2, 3, 4]]

  it "perm" $ do
    permutations ""    `shouldMatchList` L.permutations ""
    permutations "a"   `shouldMatchList` L.permutations "a"
    permutations "abc" `shouldMatchList` L.permutations "abc"
    permutations "aab" `shouldMatchList` L.permutations "aab"
