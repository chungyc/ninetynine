{- |
Description: Tree representation with s-expressions
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P73" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P73 (treeToSexp,sexpToTree) where

import           Problems.MultiwayTrees

-- | An [s-expression](https://en.wikipedia.org/wiki/S-expression) is
-- commonly represented as a list of items between parentheses.
-- In particular, s-expressions can be nested, e.g., @(a b (c d) e (f g))@.
-- It is used by programming languages such as [Lisp](https://lisp-lang.org/)
-- and [Scheme](https://groups.csail.mit.edu/mac/projects/scheme/).
--
-- A possible way to represent a multiway tree with an s-expression is for
-- the first element in a list to represent the root of the tree,
-- and the rest of the elements in the list would be its children.
-- As a special case, a multiway tree with no children is represented without parentheses.
--
-- Write a function which will return this s-expression representation
-- of a multiway tree as a string.
treeToSexp :: MultiwayTree Char -> String
treeToSexp (MultiwayTree x []) = [x]
treeToSexp (MultiwayTree x ts) = "(" ++ [x] ++ " " ++ unwords (map treeToSexp ts) ++ ")"

-- | Write a function which will convert an s-expression, representing
-- a multiway tree as in 'treeToSexp', into a 'MultiwayTree'.
sexpToTree :: String -> Maybe (MultiwayTree Char)
sexpToTree = tree . parse
  where tree Nothing           = Nothing
        tree (Just (t,[]))     = Just t
        tree (Just (t,' ':xs)) = tree $ Just (t,xs)
        tree (Just (_,_))      = Nothing

parse :: String -> Maybe (MultiwayTree Char, String)
parse []       = Nothing
parse (')':_)  = Nothing
parse (' ':xs) = parse xs
parse ('(':xs) = do
  (x,xs') <- parseNode xs
  (ts,xs'') <- parseList ([],xs')
  return (MultiwayTree x ts, xs'')
parse (x:xs) = Just (MultiwayTree x [],xs)

parseNode :: String -> Maybe (Char, String)
parseNode []       = Nothing
parseNode ('(':_)  = Nothing
parseNode (')':_)  = Nothing
parseNode (' ':xs) = parseNode xs
parseNode (x:xs)   = Just (x, xs)

parseList :: ([MultiwayTree Char], String) -> Maybe ([MultiwayTree Char], String)
parseList (_,[]) = Nothing
parseList (ts,')':xs) = Just (reverse ts,xs)
parseList (ts,xs) = do
  (t,xs') <- parse xs
  parseList (t:ts,xs')
