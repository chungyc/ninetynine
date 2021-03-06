module Problems.P58Bench (group) where

import           Criterion
import           Problems.BinaryTrees
import qualified Problems.P58         as Problem
import qualified Solutions.P58        as Solution

group :: Benchmark
group = bgroup "P58"
  [ subgroup "symmetricBalancedTrees" Problem.symmetricBalancedTrees
  , bgroup "Solutions"
    [ subgroup "symmetricBalancedTrees" Solution.symmetricBalancedTrees ]
  ]

subgroup :: String -> (Int -> [Tree Char]) -> Benchmark
subgroup name symmetricBalancedTrees = bgroup name
  [ bench  "4" $ nf symmetricBalancedTrees  4
  , bench "20" $ nf symmetricBalancedTrees 20
  ]
