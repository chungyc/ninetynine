{- |
Description: 'myButLast'

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P02".
-}
module Problems.P02 (myButLast) where

import qualified Solutions.P02 as Solution

-- | Find the last but one element of a list.
--
-- Examples:
--
-- >>> myButLast [1,2,3,4]
-- 3
--
-- >>> myButLast ['a'..'z']
-- 'y'
myButLast :: [a] -> a
myButLast = Solution.myButLast
