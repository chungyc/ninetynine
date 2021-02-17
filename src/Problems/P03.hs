{- |
Description: 'elementAt'

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P03".
-}
module Problems.P03 (elementAt) where

import qualified Solutions.P03 as Solution

-- | Find the K'th element of a list.
-- The first element in the list is number 1.
--
-- Examples:
--
-- >>> elementAt [1,2,3] 2
-- 2
--
-- >>> elementAt "haskell" 5
-- 'e'
--
-- &#129335;
elementAt :: [a] -> Int -> a
elementAt = Solution.elementAt
