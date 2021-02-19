{- |
Description: 'split'

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P17".
-}
module Problems.P17 (split) where

import qualified Solutions.P17 as Solution

-- | Split a list into two parts; the length of the first part is given.
--
-- Do not use any predefined predicates.
--
-- Example:
--
-- >>> split "abcdefghik" 3
-- ("abc","defghik")
--
-- &#129335;
split :: [a] -> Int -> ([a], [a])
split = Solution.split
