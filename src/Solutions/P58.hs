{- |
Description: `symmetricBalancedTrees`

Some solutions to "Problems.P58" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P58 (symmetricBalancedTrees) where

import           Problems.BinaryTrees
import           Problems.P55
import           Problems.P56

-- | Construct all symmetric, completely balanced binary trees with a given number of nodes.
symmetricBalancedTrees :: Int -> [Tree Char]
symmetricBalancedTrees = filter symmetric . completelyBalancedTrees
