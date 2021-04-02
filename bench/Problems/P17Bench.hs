{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P17Bench (group) where

import           Criterion
import qualified Problems.P17  as Problem
import qualified Solutions.P17 as Solution

group :: Benchmark
group = bgroup "P17"
  [ subgroup "split" Problem.split
  , bgroup "Solutions"
    [ subgroup "split" Solution.split ]
  ]

subgroup :: String -> ([Int] -> Int -> ([Int], [Int])) -> Benchmark
subgroup name split = bgroup name
  [ bench "10"   $  nf (split [1..1000]) 10
  , bench "500"  $  nf (split [1..1000]) 500
  , bench "900"  $  nf (split [1..1000]) 900
  ]
