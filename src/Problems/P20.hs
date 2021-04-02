{- |
Description: 'removeAt'
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P20".
-}
module Problems.P20 (removeAt) where

import qualified Solutions.P20 as Solution

-- | Remove the @k@th element from a list.
-- Return the element removed and the residue list.
--
-- === Examples
--
-- >>> removeAt 2 "abcd"
-- ('b',"acd")
removeAt :: Int -> [a] -> (a,[a])
removeAt = Solution.removeAt
