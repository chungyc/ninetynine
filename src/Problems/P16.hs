{- |
Description: 'dropEvery'
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P16".
-}
module Problems.P16 (dropEvery) where

import qualified Solutions.P16 as Solution

-- | Drop every N'th element from a list.
--
-- === Examples
--
-- >>> dropEvery "abcdefghik" 3
-- "abdeghk"
dropEvery :: [a] -> Int -> [a]
dropEvery = Solution.dropEvery
