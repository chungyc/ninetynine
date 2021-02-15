-- | Part of Ninety-Nine Haskell "Problems".
module Problems.P01 (myLast) where

import qualified Solutions.P01 as Solution

-- | Find the last element of a list.
--
-- Examples:
--
-- >>> myLast [1,2,3,4]
-- 4
--
-- >>> myLast ['x','y','z']
-- 'z'
myLast :: [a] -> a
myLast = Solution.myLast
