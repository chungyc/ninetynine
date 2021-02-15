-- | A solution to "Problems.P15" of Ninety-Nine Haskell "Problems".
module Solutions.P15 (repli) where

-- | Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli _ 0      = []
repli [] _     = []
repli (x:xs) n = prepend x n (repli xs n)

prepend :: a -> Int -> [a] -> [a]
prepend _ 0 l = l
prepend x n l
  | n > 0 = prepend x (n-1) (x:l)
  | otherwise = undefined
