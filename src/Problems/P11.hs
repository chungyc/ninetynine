module Problems.P11 (encodeModified) where

import           Problems.P11Definitions
import qualified Solutions.P11           as Solution

-- | Modified run-length encoding.
--
-- Modify the result of problem 10 in such a way that if an element has
-- no duplicates it is simply copied into the result list.
-- Only elements with duplicates are transferred as (N, E) tuples.
encodeModified :: (Eq a) => [a] -> [Encoding a]
encodeModified = Solution.encodeModified
