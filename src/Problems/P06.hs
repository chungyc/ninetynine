module Problems.P06 (isPalindrome) where

import qualified Solutions.P06 as Solution

-- | Find out whether a list is a palindrome.
-- A palindrome can be read forward or backward; e.g. "xamax".
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome = Solution.isPalindrome
