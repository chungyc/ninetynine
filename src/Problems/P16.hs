{- |
Description: 'dropEvery'

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P16".
-}
module Problems.P16 (dropEvery) where

import qualified Solutions.P16 as Solution

-- | Drop every N'th element from a list.
--
-- Example:
--
-- >>> dropEvery "abcdefghik" 3
-- "abdeghk"
--
-- &#129335; &#129335;
dropEvery :: [a] -> Int -> [a]
dropEvery = Solution.dropEvery
