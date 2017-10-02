module Task.Split ( splitOn
                  , joinWith
                  ) where

splitOn :: Char -> String -> [String]
splitOn splitter = foldr helper [""]
  where
    helper :: Char -> [String] -> [String]
    helper c (r:rs) | splitter == c = "":r:rs
                    | otherwise     = (c:r):rs

joinWith :: Char -> [String] -> String
joinWith _      [] = ""
joinWith joiner xs = foldr1 helper xs
  where
    helper :: String -> String -> String
    helper str acc = str ++ joiner:acc