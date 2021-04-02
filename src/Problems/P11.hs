{- |
Description: 'encodeModified'
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P11".
-}
module Problems.P11 (encodeModified) where

import           Problems.Lists
import qualified Solutions.P11  as Solution

-- | Modified run-length encoding.
--
-- Modify the 'Problems.P10.encode' function in such a way that
-- if an element has no duplicates it is simply copied into the result list.
-- Only elements with duplicates are transferred as @('Multiple' n x)@ values.
--
-- === Examples
--
-- >>> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
encodeModified :: Eq a => [a] -> [Encoding a]
encodeModified = Solution.encodeModified
