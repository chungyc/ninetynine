{- |
Description:  Construct completely balanced binary trees
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P55" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P55 (completelyBalancedTrees) where

import           Problems.BinaryTrees

-- | Constructs completely balanced binary trees for a given number of nodes.
completelyBalancedTrees :: Int -> [Tree ()]
completelyBalancedTrees 0 = [Empty]
completelyBalancedTrees n
  | even n    = generate (n `div` 2) ((n `div` 2) - 1) ++
                generate ((n `div` 2) - 1) (n `div` 2)
  | otherwise = generate (n `div` 2) (n `div` 2)

generate :: Int -> Int -> [Tree ()]
generate m n = [Branch () l r | l <- ltrees, r <- rtrees]
  where ltrees = completelyBalancedTrees m
        rtrees = completelyBalancedTrees n
