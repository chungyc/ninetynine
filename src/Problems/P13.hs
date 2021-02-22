{- |
Description: 'encodeDirect'

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P13".
-}
module Problems.P13 (encodeDirect) where

import           Problems.P11.Definitions
import qualified Solutions.P13            as Solution

-- | Run-length encoding of a list.  Direct solution.
--
-- Implement the so-called run-length encoding data compression method directly.
-- I.e., do not explicitly create the sublists containing the duplicates,
-- as with 'Problems.P09.pack', but only count them.
--
-- As with 'Problems.P11.encodeModified',
-- simplify the result list by replacing the singletons @('Multiple' 1 x)@ by @('Single' x)@.
--
-- Example:
--
-- >>> encodeDirect "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
encodeDirect :: Eq a => [a] -> [Encoding a]
encodeDirect = Solution.encodeDirect
