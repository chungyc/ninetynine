module Problems.P61Bench (group) where

import           Criterion
import           Problems.BinaryTrees
import           Problems.P55
import qualified Problems.P61         as Problem
import qualified Solutions.P61        as Solution

group :: Benchmark
group = bgroup "P61"
  [ subgroup "leaves" Problem.leaves
  , bgroup "Solutions"
    [ subgroup "leaves"  Solution.leaves ]
  ]

subgroup :: String -> (Tree () -> [()]) -> Benchmark
subgroup name leaves = bgroup name
  [ bench "tree size 5"  $ nf leaves (head $ completelyBalancedTrees 5)
  , bench "tree size 25" $ nf leaves (head $ completelyBalancedTrees 25)
  ]
