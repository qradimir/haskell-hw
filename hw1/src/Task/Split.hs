module Task.Split where

splitOn :: Char -> String -> [String]
splitOn splitter = foldr helper [""]
  where
    helper :: Char -> [String] -> [String]
    helper c (r:rs) | splitter == c = "":r:rs
                    | otherwise     = (c:r):rs

joinWith :: Char -> [String] -> String
joinWith joiner = foldr helper ""
  where
    helper :: String -> String -> String
    helper str acc = acc ++ [joiner] ++ str