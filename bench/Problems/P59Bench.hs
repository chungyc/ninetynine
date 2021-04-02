{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P59Bench (group) where

import           Criterion
import           Problems.BinaryTrees
import qualified Problems.P59         as Problem
import qualified Solutions.P59        as Solution

group :: Benchmark
group = bgroup "P59"
  [ subgroup "heightBalancedTrees" Problem.heightBalancedTrees
  , bgroup "Solutions" [ subgroup "heightBalancedTrees"  Solution.heightBalancedTrees ]
  ]

subgroup :: String -> (Int -> [Tree ()]) -> Benchmark
subgroup name heightBalancedTrees = bgroup name
  [ bench "4" $ nf heightBalancedTrees 4
  , bench "5" $ nf heightBalancedTrees 5
  ]
