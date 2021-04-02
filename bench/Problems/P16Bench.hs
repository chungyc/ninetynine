{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P16Bench (group) where

import           Criterion
import qualified Problems.P16  as Problem
import qualified Solutions.P16 as Solution

group :: Benchmark
group = bgroup "P16"
  [ subgroup "dropEvery" Problem.dropEvery
  , bgroup "Solutions"
    [ subgroup "dropEvery" Solution.dropEvery ]
  ]

subgroup :: String -> ([Int] -> Int -> [Int]) -> Benchmark
subgroup name dropEvery = bgroup name
  [ bench "1"   $  nf (dropEvery [1..1000]) 1
  , bench "10"  $  nf (dropEvery [1..1000]) 10
  , bench "100" $  nf (dropEvery [1..1000]) 100
  ]
