{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P28Bench (group) where

import           Criterion
import qualified Problems.P28  as Problem
import qualified Solutions.P28 as Solution

group :: Benchmark
group = bgroup "P28"
  [ subgroup "lsort" Problem.lsort
  , subgroup "lfsort" Problem.lfsort
  , bgroup "Solutions"
    [ subgroup "lsort" Solution.lsort
    , subgroup "lfsort" Solution.lfsort
    ]
  ]

subgroup :: String -> ([[Int]] -> [[Int]]) -> Benchmark
subgroup name f = bgroup name
  [ bench "[[2,4,3],[3,5,6],[2],[1],[12,52,42,64]]" $ nf f [[2,4,3],[3,5,6],[2],[1],[12,52,42,64]]
  , bench "[[1..k] | i <- [0..99], k <- replicate (100-i) i]" $ nf f [[1..k] | i <- [0..99], k <- replicate (100-i) i]
  ]
