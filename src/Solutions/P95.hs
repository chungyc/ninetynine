{- |
Description: 'fullWords'
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P95" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P95 (fullWords) where

import           Data.List (intercalate)

-- | English number words.
--
-- On financial documents such as checks, numbers must sometimes be written in full words.
-- For example, 175 must be written as @"one-seven-five"@.
-- Write a function to return non-negative integers in full words.
fullWords :: Integral a => a -> String
fullWords 0 = "zero"
fullWords n = intercalate "-" $ map toDigit $ toList n []

toList :: Integral a => a -> [a] -> [a]
toList 0 ds = ds
toList n ds = toList (n `div` 10) ((n `mod` 10) : ds)

toDigit :: Integral a => a -> String
toDigit 0 = "zero"
toDigit 1 = "one"
toDigit 2 = "two"
toDigit 3 = "three"
toDigit 4 = "four"
toDigit 5 = "five"
toDigit 6 = "six"
toDigit 7 = "seven"
toDigit 8 = "eight"
toDigit 9 = "nine"
toDigit _ = undefined
