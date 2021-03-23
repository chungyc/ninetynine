{- |
Description: `stringToMultitree`

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P70".
-}
module Problems.P70 (stringToMultitree, multitreeToString) where

import           Problems.MultiwayTrees
import qualified Solutions.P70          as Solution

-- | Tree construction from a node string.
--
-- We suppose that the nodes of a multiway tree contain single characters.
-- The characters in the node string are in depth-first order of the tree.
-- The special character @^@ is inserted whenever the move is
-- a backtrack to the previous level during tree traversal.
-- Note that the tree traversal will also backtrack from the root node of the tree.
--
-- For example, 'multitree5' is represented by the string @"afg^^c^bd^e^^^"@.
--
-- Write a function to construct the 'MultiwayTree' when the string is given.
--
-- === Examples
--
-- >>> stringToMultitree "afg^^c^bd^e^^^" == multitree5
-- True
stringToMultitree :: String -> MultiwayTree Char
stringToMultitree = Solution.stringToMultitree

-- | Construct the node string from a 'MultiwayTree'.
--
-- === Examples
--
-- >>> multitreeToString multitree5
-- "afg^^c^bd^e^^^"
multitreeToString :: MultiwayTree Char -> String
multitreeToString = Solution.multitreeToString
