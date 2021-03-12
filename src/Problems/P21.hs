{- |
Description: 'insertAt'

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P21".
-}
module Problems.P21 (insertAt) where

import qualified Solutions.P21 as Solution

-- | Insert an element at a given position into a list.
--
-- === Examples
--
-- >>> insertAt 'X' "abcd" 2
-- "aXbcd"
insertAt :: a -> [a] -> Int -> [a]
insertAt = Solution.insertAt
