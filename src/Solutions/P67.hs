{- |
Description: 'treeToString'

Some solutions to "Problems.P67" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P67 (treeToString, stringToTree) where

import           Problems.BinaryTrees

{- |
A string representation of binary trees.

Somebody represents binary trees as strings of the following form:

> a(b(d,e),c(,f(g,)))

Write a function to generate this string representation from a binary tree.
-}
treeToString :: Tree Char -> String
treeToString Empty = ""
treeToString (Branch c Empty Empty) = [c]
treeToString (Branch c l r) = [c] ++ "(" ++ treeToString l ++ "," ++ treeToString r ++ ")"

-- | Write a function to construct a tree from the string representation.
stringToTree :: String -> Tree Char
stringToTree ""  = Empty
stringToTree [c] = Branch c Empty Empty
stringToTree s = t
  where ("", t) = parseTree s

parseTree :: String -> (String, Tree Char)
parseTree ""             = ("", Empty)
parseTree cs@(',':_)     = (cs, Empty)
parseTree cs@(')':_)     = (cs, Empty)
parseTree [c]            = ("", Branch c Empty Empty)
parseTree (c:cs@(',':_)) = (cs, Branch c Empty Empty)
parseTree (c:cs@(')':_)) = (cs, Branch c Empty Empty)
parseTree (c:'(':cs)     = (cs'', Branch c l r)
  where (',':cs',l) = parseTree cs
        (')':cs'',r) = parseTree cs'
parseTree _ = undefined
