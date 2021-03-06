{- |
Description: 'goldbach'

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P40".
-}
module Problems.P40 (goldbach) where

import qualified Solutions.P40 as Solution

-- | Goldbach's conjecture.
--
-- Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers.
-- For example: \(28 = 5 + 23\).
-- It is one of the most famous facts in number theory that has not been proved to be correct in the general case.
-- It has been numerically confirmed up to very large numbers.
--
-- Write a predicate to find the two prime numbers that sum up to a given even integer.
--
-- === Examples
--
-- >>> goldbach 12
-- (5,7)
goldbach :: Integral a => a -> (a,a)
goldbach = Solution.goldbach
