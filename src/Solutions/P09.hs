module Solutions.P09 (pack) where

-- | Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements, they should be placed in separate sublists.
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs = duplicates : pack remainder
  where (duplicates, remainder) = extract xs

-- | Extract consecutive duplicates in front of the list into their own list.
-- Returns the consecutive duplicates in the first element,
-- and the remainder of the list in the second element.
extract :: (Eq a) => [a] -> ([a], [a])
extract (x : ys@(y : _))
  | x == y    = let (d, r) = extract ys in (x : d, r)
  | otherwise = ([x], ys)
extract l = (l, [])
