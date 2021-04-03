{- |
Description: Tree representation with s-expressions
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P73" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P73 (treeToSexp,sexpToTree) where

import           Data.List              (intercalate)
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
treeToSexp (MultiwayTree x ts) = "(" ++ [x] ++ " " ++ (intercalate " " $ map treeToSexp ts) ++ ")"

-- | Write a function which will convert an s-expression, representing
-- a multiway tree as in 'treeToSexp', into a 'MultiwayTree'.
sexpToTree :: String -> MultiwayTree Char
sexpToTree = tree . parse
  where tree (t,[])     = t
        tree (t,' ':xs) = tree (t,xs)
        tree (_,_)      = undefined

parse :: String -> (MultiwayTree Char, String)
parse []       = undefined
parse (')':_)  = undefined
parse (' ':xs) = parse xs
parse ('(':xs) = (MultiwayTree x ts, xs'')
  where (x,xs') = parseNode xs
        (ts,xs'') = parseList ([],xs')
parse (x:xs) = (MultiwayTree x [],xs)

parseNode :: String -> (Char, String)
parseNode []       = undefined
parseNode ('(':_)  = undefined
parseNode (')':_)  = undefined
parseNode (' ':xs) = parseNode xs
parseNode (x:xs)   = (x, xs)

parseList :: ([MultiwayTree Char], String) -> ([MultiwayTree Char], String)
parseList (_,[]) = undefined
parseList (ts,')':xs) = (reverse ts,xs)
parseList (ts,xs) = parseList (t:ts,xs')
  where (t,xs') = parse xs
