{- |
Description: 'myGCD'

Some solutions to "Problems.P32" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P32 (myGCD) where

-- | Determine the greatest common divisor of two positive integer numbers.
-- Use [Euclid's algorithm](https://en.wikipedia.org/wiki/Euclidean_algorithm).
myGCD :: Integral a => a -> a -> a
myGCD a b
  | a == 0    = undefined
  | b == 0    = undefined
  | a < 0     = myGCD (-a) b
  | b < 0     = myGCD a (-b)
  | a < b     = positiveGcd b a
  | otherwise = positiveGcd a b
  where positiveGcd x y
          | y == 0    = x
          | otherwise = positiveGcd y (x `mod` y)
