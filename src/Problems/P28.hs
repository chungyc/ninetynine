{- |
Description: 'lsort'
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P28".
-}
module Problems.P28 (lsort, lfsort) where

import qualified Solutions.P28 as Solution

-- | Sorting a list of lists according to length of sublists.
--
-- We suppose that a list contains elements that are lists themselves.
-- The objective is to sort the elements of this list according to their length,
-- i.e., short lists first and longer lists later.
--
-- === Examples
--
-- >>> lsort ["xxx","xx","xxx","xx","xxxx","xx","x"]
-- ["x","xx","xx","xx","xxx","xxx","xxxx"]
lsort :: [[a]] -> [[a]]
lsort = Solution.lsort

-- | Again, we suppose that a list contains elements that are lists themselves.
-- But this time the objective is to sort the elements of this list according to their length frequency,
-- i.e., lists with rare lengths are placed first, others with a more frequent length come later.
--
-- === Examples
--
-- >>> lfsort ["xxx", "xx", "xxx", "xx", "xxxx", "xx"]
-- ["xxxx","xxx","xxx","xx","xx","xx"]
lfsort :: [[a]] -> [[a]]
lfsort = Solution.lfsort
