{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P18Bench (group) where

import           Criterion
import qualified Problems.P18  as Problem
import qualified Solutions.P18 as Solution

group :: Benchmark
group = bgroup "P18"
  [ subgroup "slice" Problem.slice
  , bgroup "Solutions"
    [ subgroup "slice" Solution.slice
    , subgroup "slice'" Solution.slice'
    ]
  ]

subgroup :: String -> ([Int] -> Int -> Int -> [Int]) -> Benchmark
subgroup name slice = bgroup name
  [ bench "[1..10000]   10  100"  $  nf (slice [1..10000]   10)  100
  , bench "[1..10000] 1000 4000"  $  nf (slice [1..10000] 1000) 4000
  , bench "[1..10000] 9980 9991"  $  nf (slice [1..10000] 9980) 9991
  ]
