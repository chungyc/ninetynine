{-# LANGUAGE DeriveGeneric #-}

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
  -- * Problem 7
  NestedList (Elem, List),
  flatten,
  -- * Problem 8
  compress,
  ) where

import           GHC.Generics (Generic)

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

-- | A list type with arbitrary nesting of lists.
data NestedList a = Elem a | List [NestedList a]
  deriving (Show, Generic)

-- | Flatten a nested list structure.
--
-- Transform a list, possibly holding lists as elements,
-- into a `flat' list by replacing each list with its elements (recursively).
flatten :: NestedList a -> [a]
flatten (Elem x)  = [x]
flatten (List xs) = concat $ map flatten xs

-- | Eliminate consecutive duplicates of list elements.
--
-- If a list contains repeated elements, they should be replaced with a single copy of the element.
-- The order of the elements should not be changed.
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:ys@(y:_))
  | x == y    = compress ys
  | otherwise = x : compress ys
