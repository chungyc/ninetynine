module Problems.P22Bench (group) where

import           Criterion
import qualified Problems.P22  as Problem
import qualified Solutions.P22 as Solution

group :: Benchmark
group = bgroup "P22"
  [ subgroup "range" Problem.range
  , bgroup "Solutions"
    [ subgroup "range" Solution.range ]
  ]

subgroup :: String -> (Int -> Int -> [Int]) -> Benchmark
subgroup name range = bgroup name
  [ bench "5 10"   $  nf (range 5) 10
  , bench "100 1000" $  nf (range 100) 1000
  ]
