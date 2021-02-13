module Problems.P09 (pack) where

import qualified Solutions.P09 as Solution

-- | Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements, they should be placed in separate sublists.
pack :: (Eq a) => [a] -> [[a]]
pack = Solution.pack
