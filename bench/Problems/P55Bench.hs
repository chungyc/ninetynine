module Problems.P55Bench (group) where

import           Criterion
import           Problems.P54.Definitions
import qualified Problems.P55             as Problem
import qualified Solutions.P55            as Solution

group :: Benchmark
group = bgroup "P55"
  [ subgroup "cbalTree" Problem.cbalTree
  , bgroup "Solutions"
    [ subgroup "cbalTree" Solution.cbalTree ]
  ]

subgroup :: String -> (Int -> [Tree Char]) -> Benchmark
subgroup name cbalTree = bgroup name
  [ bench  "4" $ nf cbalTree  4
  , bench "20" $ nf cbalTree 20
  ]
