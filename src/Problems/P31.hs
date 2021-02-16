{- |
Description: 'isPrime'

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P31".
-}
module Problems.P31 (isPrime) where

import qualified Solutions.P31 as Solution

-- | Determine whether a given integer number is prime.
--
-- Example:
--
-- >>> isPrime 7
-- True
--
-- >>> isPrime 8
-- False
isPrime :: Integral a => a -> Bool
isPrime = Solution.isPrime
