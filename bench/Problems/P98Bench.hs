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
  [ bench "nonogram rows columns" $ nf (Problem.nonogram rows) columns
  , bgroup "Solutions"
    [ bench "nonogram rows columns" $ nf (Solution.nonogram rows) columns ]
  ]

rows :: [[Int]]
rows = [[3],[2,1],[3,2],[2,2],[6],[1,5],[6],[1],[2]]

columns :: [[Int]]
columns = [[1,2],[3,1],[1,5],[7,1],[5],[3],[4],[3]]
