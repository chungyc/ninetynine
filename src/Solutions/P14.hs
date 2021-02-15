-- | Solution to one of Ninety-Nine Haskell "Problems".
module Solutions.P14 (dupli) where

-- | Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = x : x : dupli xs
