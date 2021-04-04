{- |
Description: Truth tables for \(n\)-ary boolean functions
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P48".
-}
module Problems.P48 (tablen) where

import qualified Solutions.P48 as Solution

-- $setup
-- >>> import           Problems.Logic
-- >>> import           Problems.P46

-- | Truth tables for logical expressions with an arbitrary number of variables.
--
-- Generalize 'Problems.P46.table' in a way that the logical expression may contain any number of logical variables.
-- Define 'tablen' such that @tablen n f@ prints the truth table for the expression @f@,
-- which contains @n@ logical variables.
--
-- === Examples
--
-- >>> printTablen $ tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
-- False False False False
-- False False True  False
-- False True  False False
-- False True  True  True
-- True  False False False
-- True  False True  True
-- True  True  False False
-- True  True  True  True
tablen :: Int -> ([Bool] -> Bool) -> [[Bool]]
tablen = Solution.tablen
