{- |
Description: Random permutation of a list
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P25".
-}
module Problems.P25 (randomPermute) where

import qualified Solutions.P25 as Solution
import           System.Random

-- $setup
-- >>> import Data.List (unfoldr)

-- | Generate a random permutation of the elements of a list.
--
-- === Examples
--
-- >>> fst $ randomPermute [1..10] $ mkStdGen 111
-- [8,4,7,9,3,5,10,2,1,6]
--
-- >>> take 5 $ unfoldr (Just . randomPermute ['a'..'d']) $ mkStdGen 111
-- ["cbad","abdc","abdc","acdb","cdba"]
--
-- >>> fst . randomPermute "abcdef" <$> newStdGen
-- "dcaebf"
randomPermute :: RandomGen g => [a] -> g -> ([a], g)
randomPermute = Solution.randomPermute
