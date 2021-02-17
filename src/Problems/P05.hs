{- |
Description: 'myReverse'

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P05".
-}
module Problems.P05 (myReverse) where

import qualified Solutions.P05 as Solution

-- | Reverse a list.
--
-- Examples:
--
-- >>> myReverse "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
--
-- >>> myReverse [1,2,3,4]
-- [4,3,2,1]
--
-- &#129335;
myReverse :: [a] -> [a]
myReverse = Solution.myReverse
