{- |
Description: 'slice'

Some solutions to "Problems.P18" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P18 (slice,slice') where

-- | Extract a slice from a list.
--
-- Given two indices, @i@ and @k@, the slice is the list containing the elements
-- between the @i@'th and @k@'th element of the original list (both limits included).
-- Start counting the elements with 1.
--
-- Go through individual elements in the list, dropping them and then keeping some of the rest.
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice xs 1 m = extract xs m
slice (_:xs) n m
  | n > 1     = slice xs (n-1) (m-1)
  | otherwise = undefined

extract :: [a] -> Int -> [a]
extract [] _ = []
extract (x:_) 1 = [x]
extract (x:xs) n
  | n > 0     = x : extract xs (n-1)
  | otherwise = undefined

-- | Extract a slice from a list.
--
-- Given two indices, @i@ and @k@, the slice is the list containing the elements
-- between the @i@'th and @k@'th element of the original list (both limits included).
-- Start counting the elements with 1.
--
-- 'drop' and then 'take'.
slice' :: [a] -> Int -> Int -> [a]
slice' l n m = take (m-n+1) $ drop (n-1) l
