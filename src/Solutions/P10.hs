module Solutions.P10 (encode) where

import           Problems.P04
import           Problems.P09

-- | Run-length encoding of a list.
--
-- Consecutive duplicates of elements are encoded as tuples @(n, e)@,
-- where @n@ is the number of duplicates of the element @e@.
--
-- Use the 'Problems.P09.pack' function to implement
-- the so-called run-length encoding data compression method.
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\x -> (myLength x, head x)) $ pack xs
