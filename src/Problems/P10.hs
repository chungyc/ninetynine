module Problems.P10 (encode) where

import qualified Solutions.P10 as Solution

-- | Run-length encoding of a list.
-- Use the pack function from problem 9 to implement
-- the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as tuples (N, E),
-- where N is the number of duplicates of the element E.
encode :: (Eq a) => [a] -> [(Int, a)]
encode = Solution.encode
