{- |
Description: Duplicate elements in a list
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P14".
-}
module Problems.P14 (dupli) where

import qualified Solutions.P14 as Solution

-- | Duplicate the elements of a list.
--
-- === Examples
--
-- >>> dupli [1, 2, 3]
-- [1,1,2,2,3,3]
dupli :: [a] -> [a]
dupli = Solution.dupli
