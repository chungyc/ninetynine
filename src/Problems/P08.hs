-- | Part of Ninety-Nine Haskell "Problems".
module Problems.P08 (compress) where

import qualified Solutions.P08 as Solution

-- | Eliminate consecutive duplicates of list elements.
--
-- If a list contains repeated elements,
-- they should be replaced with a single copy of the element.
-- The order of the elements should not be changed.
--
-- Example:
-- >>> compress "aaaabccaadeeee"
-- "abcade"
compress :: (Eq a) => [a] -> [a]
compress = Solution.compress
