{- |
Description: 'isPalindrome'

Some solutions to "Problems.P06" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P06 (isPalindrome) where

import           Problems.P05

-- | Find out whether a list is a palindrome.
-- A palindrome can be read forward or backward; e.g. "xamax".
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == myReverse xs
