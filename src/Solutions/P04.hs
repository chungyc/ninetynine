-- | A solution to "Problems.P04" of Ninety-Nine Haskell "Problems".
module Solutions.P04 (myLength, myLength', myLength'') where

-- | Find the number of elements of a list.
--
-- Add 1 for each element by induction.
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

-- | Find the number of elements of a list.
--
-- Cheat by using 'length'.
myLength' :: [a] -> Int
myLength' = length

-- | Find the number of elements of a list.
--
-- Map elements to 1 and return their sum.
myLength'' :: [a] -> Int
myLength'' l = sum $ map (const 1) l
