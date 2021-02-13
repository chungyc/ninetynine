module Solutions.P10 (encode) where

import           Problems.P04
import           Problems.P09

-- | Run-length encoding of a list.
-- Use the pack function from problem 9 to implement
-- the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as tuples (N, E),
-- where N is the number of duplicates of the element E.
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\x -> (myLength x, head x)) $ pack xs
