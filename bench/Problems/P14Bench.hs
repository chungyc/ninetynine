module Problems.P14Bench (group) where

import           Criterion
import qualified Problems.P14  as Problem
import qualified Solutions.P14 as Solution

group :: Benchmark
group = bgroup "P14"
  [ subgroup "dupli" Problem.dupli
  , bgroup "Solutions"
    [ subgroup "dupli" Solution.dupli ]
  ]

subgroup :: String -> ([Int] -> [Int]) -> Benchmark
subgroup name dupli = bgroup name
  [ bench "[1]"       $ nf dupli [1]
  , bench "[1..10]"   $ nf dupli [1..10]
  , bench "[1..100]"  $ nf dupli [1..100]
  , bench "[1..1000]" $ nf dupli [1..1000]
  ]
