{- |
Description: Collect nodes at a given level
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P62" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P62 (atLevel) where

import           Problems.BinaryTrees

-- | Collect the nodes at a given level in a list
atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch x _ _) 1 = [x]
atLevel (Branch _ l r) n
  | n > 1     = atLevel l (n-1) ++ atLevel r (n-1)
  | otherwise = undefined
