module Task.Lists where

removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ []                 = (Nothing, [])
removeAt n (x:xs) | n > 0     = let (r, rs) = removeAt (n - 1) xs in (r, x:rs)
                  | n == 0    = (Just x, xs)
                  | otherwise = (Nothing, x:xs)

collectEvery :: Int -> [a] -> ([a], [a])
collectEvery n xs | n > 1     = collectEveryAcc (n - 1) (n - 1) xs
                  | n == 1    = ([], xs)
                  | otherwise = (xs, [])
  where
    collectEveryAcc acc n []     = ([], [])
    collectEveryAcc 0   n (x:xs) = let (ls, rs) = collectEveryAcc n         n xs in (ls, x:rs)
    collectEveryAcc acc n (x:xs) = let (ls, rs) = collectEveryAcc (acc - 1) n xs in (x:ls, rs)

stringSum :: String -> Int
stringSum = sum . map read . words

mergeSort :: Ord a => [a] -> [a]
mergeSort x =  let (l, r) = collectEvery 2 x in merge (mergeSort l) (mergeSort r)
  where
    merge :: Ord a => [a] -> [a] -> [a]
    merge []     r      = r
    merge l      []     = l
    merge (l:ls) (r:rs) | l <= r    = l : merge ls (r:rs)
                        | otherwise = r : merge (l:ls) rs
