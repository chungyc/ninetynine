{- |
Description: `symmetricBalancedTrees`
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P58" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P58 (symmetricBalancedTrees) where

import           Problems.BinaryTrees
import           Problems.P55
import           Problems.P56

-- | Construct all symmetric, completely balanced binary trees with a given number of nodes.
symmetricBalancedTrees :: Int -> [Tree ()]
symmetricBalancedTrees = filter symmetric . completelyBalancedTrees
