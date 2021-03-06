module Problems.P55Bench (group) where

import           Criterion
import           Problems.BinaryTrees
import qualified Problems.P55         as Problem
import qualified Solutions.P55        as Solution

group :: Benchmark
group = bgroup "P55"
  [ subgroup "completelyBalancedTrees" Problem.completelyBalancedTrees
  , bgroup "Solutions"
    [ subgroup "completelyBalancedTrees" Solution.completelyBalancedTrees ]
  ]

subgroup :: String -> (Int -> [Tree Char]) -> Benchmark
subgroup name completelyBalancedTrees = bgroup name
  [ bench  "4" $ nf completelyBalancedTrees  4
  , bench "20" $ nf completelyBalancedTrees 20
  ]
