{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P94Bench (group) where

import           Criterion
import           Problems.Graphs
import qualified Problems.P94    as Problem
import qualified Solutions.P94   as Solution

group :: Benchmark
group = bgroup "P94"
  [ subgroup "regularGraphs" Problem.regularGraphs
  , bgroup "Solutions"
    [ subgroup "regularGraphs"  Solution.regularGraphs ]
  ]

subgroup :: String -> (Int -> Int -> [G]) -> Benchmark
subgroup name regularGraphs = bgroup name
  [ bench "6 3"     $ nf (regularGraphs 6) 3
  , bench "8 3"     $ nf (regularGraphs 8) 3
  ]
