{- |
Description: Penultimate element of a list
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P02".
-}
module Problems.P02 (myButLast) where

import qualified Solutions.P02 as Solution

-- | Find the last but one element of a list.
--
-- === Examples
--
-- >>> myButLast [1,2,3,4]
-- Just 3
--
-- >>> myButLast ['a'..'z']
-- Just 'y'
--
-- >>> myButLast ['a']
-- Nothing
myButLast :: [a] -> Maybe a
myButLast = Solution.myButLast
