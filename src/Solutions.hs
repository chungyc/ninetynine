{- |
Description : Ninety-Nine Haskell Solutions
Maintainer  : dev@chungyc.org

Solutions to the [99 Haskell Problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems).
-}

module Solutions (
  -- * Problem 1
  myLast,
  -- * Problem 2
  myButLast,
  -- * Problem 3
  elementAt,
  -- * Problem 4
  myLength,
  -- * Problem 5
  myReverse,
  -- * Problem 6
  isPalindrome,
  ) where

-- | Find the last element of a list.
myLast :: [a] -> a
myLast []     = undefined
myLast [x]    = x
myLast (_:xs) = myLast xs

-- | Find the last but one element of a list.
myButLast :: [a] -> a
myButLast []     = undefined
myButLast [_]    = undefined
myButLast [x,_]  = x
myButLast (_:xs) = myButLast xs

-- | Find the K'th element of a list.
-- The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt [] _ = undefined
elementAt (x:xs) k
  | k == 1    = x
  | k > 1     = elementAt xs (k-1)
  | otherwise = undefined

-- | Find the number of elements of a list.
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

-- | Reverse a list.
myReverse :: [a] -> [a]
myReverse xs = snd $ accumulateReverse (xs, [])

accumulateReverse :: ([a], [a]) -> ([a], [a])
accumulateReverse ([], xs)   = ([], xs)
accumulateReverse (x:xs, ys) = accumulateReverse (xs, x:ys)

-- | Find out whether a list is a palindrome.
-- A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs
