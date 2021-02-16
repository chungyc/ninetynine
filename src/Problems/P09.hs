{- |
Description: 'pack'

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P09".
-}
module Problems.P09 (pack) where

import qualified Solutions.P09 as Solution

-- | Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements, they should be placed in separate sublists.
--
-- Example:
--
-- >>> pack "aaaabccaadeeee"
-- ["aaaa","b","cc","aa","d","eeee"]
pack :: Eq a => [a] -> [[a]]
pack = Solution.pack
