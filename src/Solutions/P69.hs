{- |
Description: Dotstring representation of binary trees
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P69" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P69 (dotstringToTree, treeToDotstring) where

import           Control.Monad
import           Problems.BinaryTrees

-- | Convert a dotstring representation into its corresponding binary tree.
dotstringToTree :: String -> Maybe (Tree Char)
dotstringToTree s = do
  (t, s') <- parseDotstring s
  guard $ null s'
  return t

parseDotstring :: String -> Maybe (Tree Char, String)
parseDotstring ('.':xs) = Just (Empty, xs)
parseDotstring (c:xs) = do
  (l, xs') <- parseDotstring xs
  (r, xs'') <- parseDotstring xs'
  return (Branch c l r, xs'')
parseDotstring _ = Nothing

-- | Convert a binary tree to its dotstring representation.
treeToDotstring :: Tree Char -> String
treeToDotstring Empty          = "."
treeToDotstring (Branch x l r) = [x] ++ treeToDotstring l ++ treeToDotstring r
