module Problems.P95Bench (group) where

import           Criterion
import qualified Problems.P95  as Problem
import qualified Solutions.P95 as Solution

group :: Benchmark
group = bgroup "P95"
  [ subgroup "fullWords" Problem.fullWords
  , bgroup "Solutions"
    [ subgroup "fullWords" Solution.fullWords ]
  ]

subgroup :: String -> (Integer -> String) -> Benchmark
subgroup name fullWords = bgroup name
  [ bench "2002848517659248292103"   $ nf fullWords 2002848517659248292103
  , bench "2002848517659248292103^2" $ nf fullWords (2002848517659248292103^(2 :: Integer))
  ]
