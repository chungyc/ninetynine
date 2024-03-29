{- |
Description: List of prime numbers
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P39" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P39 (primesR, primes) where

import qualified Solutions.Arithmetic as Arithmetic

-- | Given a range of integers by its lower and upper limit, inclusive,
-- construct a list of all prime numbers in that range.
primesR :: Integral a => a -> a -> [a]
primesR lo hi = takeWhile (<= hi) $ dropWhile (< lo) primes

-- | Construct the list of all prime numbers.
primes :: Integral a => [a]
primes = Arithmetic.primes  -- Already done as part of a solution to problem 31.
