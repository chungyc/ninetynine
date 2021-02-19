module Problems.P90Bench (group) where

import           Criterion
import qualified Problems.P90  as Problem
import qualified Solutions.P90 as Solution

group :: Benchmark
group = bgroup "P90"
  [ subgroup "queens" Problem.queens
  , bgroup "Solutions"
    [ subgroup "queens" Solution.queens ]
  ]

subgroup :: String -> (Int -> [[Int]]) -> Benchmark
subgroup name queens = bgroup name
  [ bench "8"  $ nf queens 8
  , bench "13" $ nf queens 13
  ]
