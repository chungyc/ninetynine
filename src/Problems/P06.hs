{- |
Description: 'isPalindrome'

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P06".
-}
module Problems.P06 (isPalindrome) where

import qualified Solutions.P06 as Solution

-- | Find out whether a list is a palindrome.
-- A palindrome can be read forward or backward; e.g. "xamax".
--
-- Examples:
--
-- >>> isPalindrome [1,2,3]
-- False
--
-- >>> isPalindrome "madamimadam"
-- True
--
-- >>> isPalindrome [1,2,4,8,16,8,4,2,1]
-- True
--
-- &#129335;
isPalindrome :: Eq a => [a] -> Bool
isPalindrome = Solution.isPalindrome
