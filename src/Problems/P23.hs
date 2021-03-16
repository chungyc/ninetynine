{- |
Description: 'randomSelect'

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P23".
-}
module Problems.P23 (randomSelect) where

import qualified Solutions.P23 as Solution
import           System.Random

-- | Extract a given number of randomly selected elements from a list.
--
-- Also return a new random number generator so that callers
-- can avoid reusing a sequence of random numbers.
--
-- === Examples
--
-- >>> fst $ randomSelect "abcdefgh" 3 $ mkStdGen 111
-- "chd"
--
-- >>> take 5 $ unfoldr (Just . randomSelect [1..100] 3) $ mkStdGen 111
-- [[11,19,76],[63,49,10],[75,42,12],[20,48,78],[40,94,86]]
--
-- >>> newStdGen >>= return . fst . randomSelect "abcdefgh" 3
-- "ebf"
randomSelect :: RandomGen g => [a] -> Int -> g -> ([a], g)
randomSelect = Solution.randomSelect
