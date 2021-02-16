{- |
Description: 'isPrime'

Some solutions to "Problems.P31" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P31 (isPrime) where

-- | Determine whether a given integer number is prime.
isPrime :: Integral a => a -> Bool
isPrime n
  | n == 1         = False
  | n == 2         = True
  | n > 2          = null $ filter (\k -> n `mod` k == 0) $ 2 : odds
  | otherwise      = undefined
  where odds = takeWhile (\k -> k*k <= n) $ iterate (2+) 3
