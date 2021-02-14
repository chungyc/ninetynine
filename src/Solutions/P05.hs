module Solutions.P05 (myReverse) where

-- | Reverse a list.
myReverse :: [a] -> [a]
myReverse l = accumulate l []

-- | Accumulates into the partial reversed list in the second argument
-- as extracted from the remainder of the original list in the first argument.
--
-- Returns the fully accumulated reversed list.
accumulate :: [a] -> [a] -> [a]
accumulate [] xs     = xs
accumulate (x:xs) ys = accumulate xs (x:ys)
