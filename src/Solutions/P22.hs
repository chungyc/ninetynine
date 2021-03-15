{- |
Description: 'range'

Some solutions to "Problems.P22" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P22 (range) where

-- | Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range m n = takeWhile (n >=) $ iterate (1 +) m
