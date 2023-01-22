{- |
Description: Flatten a nested list structure
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P07".
-}
module Problems.P07 (flatten, NestedList (..)) where

import           Problems.Lists
import qualified Solutions.P07  as Solution

-- | Transform a list, possibly holding lists as elements,
-- into a "flat" list by replacing each list with its elements recursively.
--
-- === Examples
--
-- >>> flatten $ Elem 5
-- [5]
--
-- >>> flatten $ List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]
-- [1,2,3,4,5]
--
-- >>> flatten $ List []
-- []
flatten :: NestedList a -> [a]
flatten = Solution.flatten
