{- |
Description: Range of integers
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P22".
-}
module Problems.P22 (range) where

import qualified Solutions.P22 as Solution

-- | Create a list containing all integers within a given range.
--
-- === Examples
--
-- >>> range 4 9
-- [4,5,6,7,8,9]
range :: Int -> Int -> [Int]
range = Solution.range
