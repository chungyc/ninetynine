{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P98Bench (group) where

import           Criterion
import qualified Problems.P98  as Problem
import qualified Solutions.P98 as Solution

group :: Benchmark
group = bgroup "P98"
  [ subgroup "nonogram" Problem.nonogram
  , bgroup "Solutions"
    [ subgroup "nonogram" Solution.nonogram ]
  ]

subgroup :: String -> ([[Int]] -> [[Int]] -> Maybe [[Bool]]) -> Benchmark
subgroup name nonogram = bgroup name
  [ bench "8x9"   $ nf (nonogram rows9)  columns8
  , bench "25x25" $ nf (nonogram rows25) columns25
  ]
  where
    rows9 = [[3],[2,1],[3,2],[2,2],[6],[1,5],[6],[1],[2]]
    columns8 = [[1,2],[3,1],[1,5],[7,1],[5],[3],[4],[3]]
    (rows25, columns25) = Problem.nonogramPuzzle
