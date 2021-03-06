{- |
Description: 'myLength'

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P04".
-}
module Problems.P04 (myLength) where

import qualified Solutions.P04 as Solution

-- | Find the number of elements of a list.
--
-- === Examples
--
-- >>> myLength [123, 456, 789]
-- 3
--
-- >>> myLength "Hello, world!"
-- 13
myLength :: [a] -> Int
myLength = Solution.myLength
