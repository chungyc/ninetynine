module Problems.P31Bench (group) where

import           Criterion
import qualified Problems.P31  as Problem
import qualified Solutions.P31 as Solution

group :: Benchmark
group = bgroup "P31"
  [ subgroup "isPrime" Problem.isPrime
  , bgroup "Solutions"
    [ subgroup "isPrime" Solution.isPrime ]
  ]

subgroup :: String -> (Integer -> Bool) -> Benchmark
subgroup name isPrime = bgroup name
  [ bench "10001"  $ nf isPrime 10001
  , bench "10111"  $ nf isPrime 10111
  , bench "103723" $ nf isPrime 103723
  , bench "103727" $ nf isPrime 103727
  ]
