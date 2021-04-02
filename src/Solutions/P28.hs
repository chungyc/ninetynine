{- |
Description: 'lsort'
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P28" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P28 (lsort, lfsort) where

import           Data.List     (sortOn)
import           Data.Map.Lazy ((!))
import qualified Data.Map.Lazy as Map

-- | Sorting a list of lists according to length of sublists.
--
-- We suppose that a list contains elements that are lists themselves.
-- The objective is to sort the elements of this list according to their length,
-- i.e., short lists first and longer lists later.
lsort :: [[a]] -> [[a]]
lsort = sortOn length

-- | Again, we suppose that a list contains elements that are lists themselves.
-- But this time the objective is to sort the elements of this list according to their length frequency,
-- i.e., lists with rare lengths are placed first, others with a more frequent length come later.
lfsort :: [[a]] -> [[a]]
lfsort xs = sortOn (\ys -> fs ! length ys) xs
  where fs = Map.fromListWith (+) $ map (\ys -> (length ys, 1 :: Int)) xs
