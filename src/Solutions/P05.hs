module Solutions.P05 (myReverse) where

-- | Reverse a list.
myReverse :: [a] -> [a]
myReverse l = snd $ accumulate (l, [])

-- | Accumulates the partial reversed list in the second element in the tuple
-- as extracted from the remainder of the original list in the first element.
--
-- Returns the fully accumulated reversed list in the second element.
accumulate :: ([a], [a]) -> ([a], [a])
accumulate ([], xs)   = ([], xs)
accumulate (x:xs, ys) = accumulate (xs, x:ys)
