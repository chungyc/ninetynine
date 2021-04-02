{- |
Description: 'pack'
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P09".
-}
module Problems.P09 (pack) where

import qualified Solutions.P09 as Solution

-- | Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements, they should be placed in separate sublists.
--
-- === Examples
--
-- >>> pack "aaaabccaadeeee"
-- ["aaaa","b","cc","aa","d","eeee"]
pack :: Eq a => [a] -> [[a]]
pack = Solution.pack
