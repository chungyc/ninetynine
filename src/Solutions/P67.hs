{- |
Description: A string representation of binary trees
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P67" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P67 (treeToString, stringToTree) where

import           Problems.BinaryTrees

{- |
Somebody represents binary trees as strings of the following form:

> a(b(d,e),c(,f(g,)))

Write a function to generate this string representation from a binary tree.
-}
treeToString :: Tree Char -> String
treeToString Empty = ""
treeToString (Branch c Empty Empty) = [c]
treeToString (Branch c l r) = [c] ++ "(" ++ treeToString l ++ "," ++ treeToString r ++ ")"

-- | Write a function to construct a tree from the string representation.
stringToTree :: String -> Maybe (Tree Char)
stringToTree ""  = Just Empty
stringToTree [c] = Just $ Branch c Empty Empty
stringToTree s | Just ("", t) <- p = Just t
               | otherwise = Nothing
  where p = parseTree s

parseTree :: String -> Maybe (String, Tree Char)
parseTree ""             = Just ("", Empty)
parseTree cs@(',':_)     = Just (cs, Empty)
parseTree cs@(')':_)     = Just (cs, Empty)
parseTree [c]            = Just ("", Branch c Empty Empty)
parseTree (c:cs@(',':_)) = Just (cs, Branch c Empty Empty)
parseTree (c:cs@(')':_)) = Just (cs, Branch c Empty Empty)
parseTree (c:'(':cs) = do
  (',':cs',l) <- parseTree cs
  (')':cs'',r) <- parseTree cs'
  return (cs'', Branch c l r)
parseTree _ = Nothing
