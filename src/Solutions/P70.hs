{- |
Description: Tree construction from a node string
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P70" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P70 (stringToMultitree, multitreeToString) where

import           Problems.MultiwayTrees

-- | We suppose that the nodes of a multiway tree contain single characters.
-- The characters in the node string are in depth-first order of the tree.
-- The special character @^@ is inserted whenever the move is
-- a backtrack to the previous level during tree traversal.
-- Note that the tree traversal will also backtrack from the root node of the tree.
--
-- Write a function to construct the 'MultiwayTree' when the string is given.
stringToMultitree :: String -> MultiwayTree Char
stringToMultitree = fst . toNode

toNode :: String -> (MultiwayTree Char, String)
toNode (x:xs) = (MultiwayTree x ts, remaining)
  where (ts, remaining) = toTreeList ([], xs)
toNode [] = undefined

toTreeList :: ([MultiwayTree Char], String) -> ([MultiwayTree Char], String)
toTreeList (ts, '^':xs) = (reverse ts, xs)
toTreeList (ts, s) = toTreeList (node : ts, remaining)
  where (node, remaining) = toNode s

-- | Construct the node string from a 'MultiwayTree'.
multitreeToString :: MultiwayTree Char -> String
multitreeToString (MultiwayTree x ts) = x : concatMap multitreeToString ts ++ "^"
