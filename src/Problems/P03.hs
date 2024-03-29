{- |
Description: Indexed element in a list
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P03".
-}
module Problems.P03 (elementAt) where

import qualified Solutions.P03 as Solution

-- | Find the @k@th element of a list.
-- The first element in the list is number 1.
--
-- === Examples
--
-- >>> elementAt [1,2,3] 2
-- Just 2
--
-- >>> elementAt "haskell" 5
-- Just 'e'
--
-- >>> elementAt [1,2] 3
-- Nothing
elementAt :: [a] -> Int -> Maybe a
elementAt = Solution.elementAt
