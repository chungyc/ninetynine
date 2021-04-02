{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P19Bench (group) where

import           Criterion
import qualified Problems.P19  as Problem
import qualified Solutions.P19 as Solution

group :: Benchmark
group = bgroup "P19"
  [ subgroup "rotate" Problem.rotate
  , bgroup "Solutions"
    [ subgroup "rotate" Solution.rotate ]
  ]

subgroup :: String -> ([Int] -> Int -> [Int]) -> Benchmark
subgroup name rotate = bgroup name
  [ bench "[1..10000]   10" $  nf (rotate [1..10000])   10
  , bench "[1..10000] 1000" $  nf (rotate [1..10000]) 1000
  , bench "[1..10000] 9980" $  nf (rotate [1..10000]) 9980
  ]
