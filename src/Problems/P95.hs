{- |
Description: English number words
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P95".
-}
module Problems.P95 (fullWords) where

import qualified Solutions.P95 as Solution

-- | On financial documents such as checks, numbers must sometimes be written in full words.
-- For example, 175 must be written as @"one-seven-five"@.
-- Write a function to return non-negative integers in full words.
--
-- === Examples
--
-- >>> fullWords 175
-- "one-seven-five"
fullWords :: Integral a => a -> String
fullWords = Solution.fullWords
