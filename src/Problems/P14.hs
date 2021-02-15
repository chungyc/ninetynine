-- | Part of Ninety-Nine Haskell "Problems".
module Problems.P14 (dupli) where

import qualified Solutions.P14 as Solution

-- | Duplicate the elements of a list.
--
-- Example:
--
-- >>> dupli [1, 2, 3]
-- [1,1,2,2,3,3]
dupli :: [a] -> [a]
dupli = Solution.dupli
