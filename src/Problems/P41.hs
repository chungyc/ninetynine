{- |
Description: 'goldbachList'
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P41".
-}
module Problems.P41 (goldbachList) where

import qualified Solutions.P41 as Solution

-- | Given a range of integers by its lower and upper limit,
-- return a list of all Goldbach compositions for the even numbers in the range.
--
-- In most cases, if an even number is written as the sum of two prime numbers, one of them is very small.
-- Very rarely, it cannot be a sum of primes which are both smaller than, say, 50.
-- Try to find out how many such cases there are in the range \([2,3000]\).
--
-- === Examples
--
-- >>> goldbachList 9 20
-- [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
--
-- >>> filter (\(m,n) -> m > 100 && n > 100) $ goldbachList 2 3000
-- [(103,2539)]
goldbachList :: Integral a => a -> a -> [(a,a)]
goldbachList = Solution.goldbachList
