module Problems.P07 (flatten) where

import           Problems.P07Definitions
import qualified Solutions.P07           as Solution

-- | Flatten a nested list structure.
--
-- Transform a list, possibly holding lists as elements,
-- into a `flat' list by replacing each list with its elements (recursively).
flatten :: NestedList a -> [a]
flatten = Solution.flatten
