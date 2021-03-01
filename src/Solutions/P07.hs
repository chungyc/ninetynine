{- |
Description: 'flatten'

Some solutions to "Problems.P07" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P07 (flatten, flatten') where

import           Problems.Lists

-- | Flatten a nested list structure.
--
-- Transform a list, possibly holding lists as elements,
-- into a "flat" list by replacing each list with its elements (recursively).
--
-- Recursively flatten lists and concatenate them together.
flatten :: NestedList a -> [a]
flatten (Elem x)  = [x]
flatten (List xs) = concat $ map flatten xs

-- | Flatten a nested list structure.
--
-- Transform a list, possibly holding lists as elements,
-- into a "flat" list by replacing each list with its elements (recursively).
--
-- Build the list starting from the last one, prepending each element one at a time.
flatten' :: NestedList a -> [a]
flatten' l = prepend' l []

-- | For each element from front to back, flatten the rest of the list and prepend the element.
prepend' :: NestedList a -> [a] -> [a]
prepend' (Elem x) xs      = x : xs
prepend' (List []) xs     = xs
prepend' (List (x:xs)) ys = prepend' x $ prepend' (List xs) ys
