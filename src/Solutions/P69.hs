{- |
Description: Dotstring representation of binary trees
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P69" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P69 (dotstringToTree, treeToDotstring) where

import           Problems.BinaryTrees

-- | Convert a dotstring representation into its corresponding binary tree.
dotstringToTree :: String -> Tree Char
dotstringToTree s | null s'   = t
                  | otherwise = undefined
  where (t, s') = parseDotstring s

parseDotstring :: String -> (Tree Char, String)
parseDotstring ('.':xs) = (Empty, xs)
parseDotstring (c:xs)   = (Branch c l r, xs'')
  where (l, xs')  = parseDotstring xs
        (r, xs'') = parseDotstring xs'
parseDotstring _ = undefined

-- | Convert a binary tree to its dotstring representation.
treeToDotstring :: Tree Char -> String
treeToDotstring Empty          = "."
treeToDotstring (Branch x l r) = [x] ++ treeToDotstring l ++ treeToDotstring r
