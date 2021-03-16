module Problems.P25Bench (group) where

import           Criterion
import qualified Problems.P25  as Problem
import qualified Solutions.P25 as Solution
import           System.Random

group :: Benchmark
group = bgroup "P25"
  [ subgroup "randomPermute" Problem.randomPermute
  , bgroup "Solutions"
    [ subgroup "randomPermute" Solution.randomPermute ]
  ]

subgroup :: String -> ([Int] -> StdGen -> ([Int], StdGen)) -> Benchmark
subgroup name randomPermute = bgroup name
  [ bench "[1..10]"   $  nf (fst . randomPermute [1..10])   (mkStdGen 23)
  , bench "[1..1000]" $  nf (fst . randomPermute [1..1000]) (mkStdGen 24)
  ]
