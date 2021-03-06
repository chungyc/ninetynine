module Problems.P58Bench (group) where

import           Criterion
import           Problems.BinaryTrees
import qualified Problems.P58         as Problem
import qualified Solutions.P58        as Solution

group :: Benchmark
group = bgroup "P58"
  [ subgroup "symCbalTrees" Problem.symCbalTrees
  , bgroup "Solutions"
    [ subgroup "symCbalTrees" Solution.symCbalTrees ]
  ]

subgroup :: String -> (Int -> [Tree Char]) -> Benchmark
subgroup name symCbalTrees = bgroup name
  [ bench  "4" $ nf symCbalTrees  4
  , bench "20" $ nf symCbalTrees 20
  ]
