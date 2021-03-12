module Problems.P21Bench (group) where

import           Criterion
import qualified Problems.P21  as Problem
import qualified Solutions.P21 as Solution

group :: Benchmark
group = bgroup "P21"
  [ subgroup "insertAt" Problem.insertAt
  , bgroup "Solutions"
    [ subgroup "insertAt" Solution.insertAt ]
  ]

subgroup :: String -> (Int -> [Int] -> Int -> [Int]) -> Benchmark
subgroup name insertAt = bgroup name
  [ bench "... [1..10000] 10"   $  nf (insertAt 5 [1..10000]) 10
  , bench "... [1..10000] 1000" $  nf (insertAt 5 [1..10000]) 1000
  , bench "... [1..10000] 9980" $  nf (insertAt 5 [1..10000]) 9980
  ]
