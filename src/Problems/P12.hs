module Problems.P12 (decodeModified) where

import           Problems.P11.Definitions
import qualified Solutions.P12            as Solution

-- | Decode a run-length encoded list.
--
-- Given a run-length code list generated as specified in problem 11.
-- Construct its uncompressed version.
decodeModified :: [Encoding a] -> [a]
decodeModified = Solution.decodeModified
