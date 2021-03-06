module Problems.P34Bench (group) where

import           Criterion
import qualified Problems.P34  as Problem
import qualified Solutions.P34 as Solution

group :: Benchmark
group = bgroup "P34"
  [ subgroup "totient" Problem.totient
  , bgroup "Solutions"
    [ subgroup "totient"         Solution.totient
    , subgroup "totientFiltered" Solution.totientFiltered
    ]
  ]

subgroup :: String -> (Integer -> Integer) -> Benchmark
subgroup name totient = bgroup name
  [ bench  "10090" $ nf totient  10090
  , bench  "84237" $ nf totient  84237
  , bench "934579" $ nf totient 934579
  ]
