module Problems.P26Bench (group) where

import           Criterion
import qualified Problems.P26  as Problem
import qualified Solutions.P26 as Solution

group :: Benchmark
group = bgroup "P26"
  [ subgroup "combinations" Problem.combinations
  , bgroup "Solutions"
    [ subgroup "combinations" Solution.combinations ]
  ]

subgroup :: String -> (Int -> [Int] -> [[Int]]) -> Benchmark
subgroup name combinations = bgroup name
  [ bench "4 [1..10]" $  nf (combinations 4) [1..10]
  , bench "7 [1..20]" $  nf (combinations 7) [1..20]
  ]
