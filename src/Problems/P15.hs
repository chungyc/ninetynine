-- | Part of Ninety-Nine Haskell "Problems".
module Problems.P15 (repli) where

import qualified Solutions.P15 as Solution

-- | Replicate the elements of a list a given number of times.
--
-- Example:
--
-- >>> repli "abc" 3
-- "aaabbbccc"
repli :: [a] -> Int -> [a]
repli = Solution.repli
