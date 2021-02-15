-- | A solution to "Problems.P11" of Ninety-Nine Haskell "Problems".
module Solutions.P11 (encodeModified) where

import           Problems.P04
import           Problems.P09
import           Problems.P11.Definitions

-- | Modified run-length encoding.
--
-- Modify the 'Problems.P10.encode' function in such a way that
-- if an element has no duplicates it is simply copied into the result list.
-- Only elements with duplicates are transferred as @('Multiple' n x)@ tuples.
encodeModified :: (Eq a) => [a] -> [Encoding a]
encodeModified xs = encodePacked $ pack xs

-- | Turn each sublist in a packed list into an 'Encoding'.
encodePacked :: [[a]] -> [Encoding a]
encodePacked []       = []
encodePacked ([x]:ls) = Single x : encodePacked ls
encodePacked (l:ls)   = Multiple (myLength l) (head l) : encodePacked ls
