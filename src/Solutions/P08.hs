module Solutions.P08 (compress) where

-- | Eliminate consecutive duplicates of list elements.
--
-- If a list contains repeated elements,
-- they should be replaced with a single copy of the element.
-- The order of the elements should not be changed.
compress :: (Eq a) => [a] -> [a]
compress (x:ys@(y:_))
  | x == y    = compress ys
  | otherwise = x : compress ys
compress l = l
