module Task.Primitives where

order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (x, y, z) | (x <= y) && (x <= z) = orderLast x y z
                 | (y <= x) && (y <= z) = orderLast y x z
                 | otherwise            = orderLast z x y
  where
    orderLast min x y | x <= y    = (min, x, y)
                      | otherwise = (min, y, x)

highestBit :: Int -> (Int, Int)
highestBit x | x > 1     = highestBitHelp (1, 0)
             | otherwise = (0, 0)
  where
    highestBitHelp (y, cy) | x >= 2 * y = highestBitHelp (2 * y, cy + 1)
                           | otherwise  = (y, cy)

smartReplicate :: [Int] -> [Int]
smartReplicate = concatMap (\x -> replicate x x)

contains :: Eq a => a -> [[a]] -> [[a]]
contains = filter . elem
