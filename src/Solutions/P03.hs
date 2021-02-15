-- | Solution to one of Ninety-Nine Haskell "Problems".
module Solutions.P03 (elementAt) where

-- | Find the K'th element of a list.
-- The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt [] _ = undefined
elementAt (x:xs) k
  | k == 1    = x
  | k > 1     = elementAt xs (k-1)
  | otherwise = undefined
