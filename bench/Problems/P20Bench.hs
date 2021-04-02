{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P20Bench (group) where

import           Criterion
import qualified Problems.P20  as Problem
import qualified Solutions.P20 as Solution

group :: Benchmark
group = bgroup "P20"
  [ subgroup "removeAt" Problem.removeAt
  , bgroup "Solutions"
    [ subgroup "removeAt" Solution.removeAt ]
  ]

subgroup :: String -> (Int -> [Int] -> (Int,[Int])) -> Benchmark
subgroup name removeAt = bgroup name
  [ bench "10   [1..10000]" $  nf (removeAt 10)   [1..10000]
  , bench "1000 [1..10000]" $  nf (removeAt 1000) [1..10000]
  , bench "9980 [1..10000]" $  nf (removeAt 9980) [1..10000]
  ]
