{- |
Description: `stringToMultitree`

Some solutions to "Problems.P70" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P70 (stringToMultitree, multitreeToString) where

import           Problems.MultiwayTrees

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
stringToMultitree :: String -> MultiwayTree Char
stringToMultitree = fst . toNode

toNode :: String -> (MultiwayTree Char, String)
toNode (x:xs) = (MultiwayTree x ts, remaining)
  where (ts, remaining) = toList ([], xs)
toNode [] = undefined

toList :: ([MultiwayTree Char], String) -> ([MultiwayTree Char], String)
toList (ts, '^':xs) = (reverse ts, xs)
toList (ts, s) = toList (node : ts, remaining)
  where (node, remaining) = toNode s

-- | Construct the node string from a 'MultiwayTree'.
multitreeToString :: MultiwayTree Char -> String
multitreeToString (MultiwayTree x ts) = x : (concat $ map multitreeToString ts) ++ "^"
