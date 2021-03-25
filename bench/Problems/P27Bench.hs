module Problems.P27Bench (group) where

import           Criterion
import qualified Problems.P27  as Problem
import qualified Solutions.P27 as Solution

group :: Benchmark
group = bgroup "P27"
  [ subgroup "disjointGroups" Problem.disjointGroups
  , bgroup "Solutions"
    [ subgroup "disjointGroups" Solution.disjointGroups ]
  ]

subgroup :: String -> ([Int] -> [Int] -> [[[Int]]]) -> Benchmark
subgroup name disjointGroups = bgroup name
  [ bench "[2,3,5] [1..10]" $  nf (disjointGroups [2,3,5]) [1..10]
  , bench "[4,4,5] [1..13]" $  nf (disjointGroups [4,4,5]) [1..13]
  ]
