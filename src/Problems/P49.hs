{- |
Description: `gray`

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P49".
-}
module Problems.P49 (gray) where

import qualified Solutions.P49 as Solution

-- | Gray codes.
--
-- An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules.
-- For example,
--
-- >>> gray 1  -- 1-bit gray code
-- ["0","1"]
--
-- >>> gray 2  -- 2-bit gray code
-- ["00","01","11","10"]
--
-- >>> gray 3  -- 3-bit gray code
-- ["000","001","011","010","110","111","101","100"]
--
-- Infer the construction rules and write a function returning the n-bit Gray code.
gray :: Int -> [String]
gray = Solution.gray
