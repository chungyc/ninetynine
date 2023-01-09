{- |
Description: Split a list
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P17".
-}
module Problems.P17 (split) where

import qualified Solutions.P17 as Solution

-- | Split a list into two parts; the length of the first part is given.
--
-- === Examples
--
-- >>> split "abcdefghik" 3
-- ("abc","defghik")
split :: [a] -> Int -> ([a], [a])
split = Solution.split
