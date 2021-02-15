module Solutions.P07 (flatten) where

import           Problems.P07.Definitions

-- | Flatten a nested list structure.
--
-- Transform a list, possibly holding lists as elements,
-- into a "flat" list by replacing each list with its elements (recursively).
flatten :: NestedList a -> [a]
flatten (Elem x)  = [x]
flatten (List xs) = concat $ map flatten xs
