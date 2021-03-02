module Problems.P49Bench (group) where

import           Criterion
import qualified Problems.P49  as Problem
import qualified Solutions.P49 as Solution

group :: Benchmark
group = bgroup "P49"
  [ bench "gray 15" $ nf Problem.gray 15
  , bgroup "From solutions" [ bench "gray 15" $ nf Solution.gray 15 ]
  ]
