{- |
Description: Construct height-balanced binary trees
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P59" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P59 (heightBalancedTrees) where

import           Problems.BinaryTrees

-- | Construct a list of all height-balanced binary trees with the given maximum height.
heightBalancedTrees :: Int -> [Tree ()]
heightBalancedTrees 0 = [Empty]
heightBalancedTrees 1 = [Branch () Empty Empty]
heightBalancedTrees n =
  combine (heightBalancedTrees $ n-1) (heightBalancedTrees $ n-1) ++
  combine (heightBalancedTrees $ n-1) (heightBalancedTrees $ n-2) ++
  combine (heightBalancedTrees $ n-2) (heightBalancedTrees $ n-1)

combine :: [Tree ()] -> [Tree ()] -> [Tree ()]
combine ls rs = [Branch () l r | l <- ls, r <- rs]
