{- |
Description: `symCbalTrees`

Some solutions to "Problems.P58" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P58 (symCbalTrees) where

import           Problems.BinaryTrees
import           Problems.P55
import           Problems.P56

-- | Construct all symmetric, completely balanced binary trees with a given number of nodes.
symCbalTrees :: Int -> [Tree Char]
symCbalTrees = filter symmetric . cbalTree
