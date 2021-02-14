module Problems.P10 (encode) where

import qualified Solutions.P10 as Solution

-- | Run-length encoding of a list.
--
-- Consecutive duplicates of elements are encoded as tuples @(n, e)@,
-- where @n@ is the number of duplicates of the element @e@.
--
-- Use the 'Problems.P09.pack' functionto implement
-- the so-called run-length encoding data compression method.
encode :: (Eq a) => [a] -> [(Int, a)]
encode = Solution.encode
