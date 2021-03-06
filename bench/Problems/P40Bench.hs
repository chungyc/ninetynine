module Problems.P40Bench (group) where

import           Criterion
import qualified Problems.P40  as Problem
import qualified Solutions.P40 as Solution

group :: Benchmark
group = bgroup "P40"
  [ subgroup "goldbach" Problem.goldbach
  , bgroup "Solutions"
    [ subgroup "goldbach" Solution.goldbach ]
  ]

subgroup :: String -> (Integer -> (Integer,Integer)) -> Benchmark
subgroup name goldbach = bgroup name
  [ bench  "84238" $ nf goldbach  84238
  , bench "934572" $ nf goldbach 934572
  ]
