module Problems.P11 (encodeModified) where

import           Problems.P11.Definitions
import qualified Solutions.P11            as Solution

-- | Modified run-length encoding.
--
-- Modify the 'Problems.P10.encode' function in such a way that
-- if an element has no duplicates it is simply copied into the result list.
-- Only elements with duplicates are transferred as @('Multiple' n x)@ tuples.
encodeModified :: (Eq a) => [a] -> [Encoding a]
encodeModified = Solution.encodeModified
