
module Task.List
     ( bin
     , combinations
     , permutations
     ) where



bin :: Int -> [[Int]]
bin 0 = [[]]
bin n = bin (n - 1) >>= \b -> [0:b, 1:b]

combinations :: Int -> Int -> [[Int]]
combinations n k = fmap reverse (combinations' k)
  where
    combinations' :: Int -> [[Int]]
    combinations' 0 = [[]]
    combinations' k' = combinations' (k'-1) >>= \x -> fmap (:x) [maxIn x + 1..n]

    maxIn :: [Int] -> Int
    maxIn [] = 0
    maxIn (x:_) = x


permutations :: [a] -> [[a]]
permutations []     = [[]]
permutations (x:xs) = permutations xs >>= \l -> help l []
  where
    help []     rs = [x:rs]
    help (l:ls) rs = (l:ls ++ x:rs) : help ls (l:rs)
