-- | Part of Ninety-Nine Haskell "Problems".
module Problems.P12 (decodeModified) where

import           Problems.P11.Definitions
import qualified Solutions.P12            as Solution

-- | Decode a run-length encoded list.
--
-- Given a run-length code list generated by 'Problems.P11.encodeModified',
-- construct its uncompressed version.
--
-- Example:
--
-- >>> decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"
decodeModified :: [Encoding a] -> [a]
decodeModified = Solution.decodeModified
