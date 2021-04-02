{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P60Bench (group) where

import           Criterion
import           Problems.BinaryTrees
import qualified Problems.P60         as Problem
import qualified Solutions.P60        as Solution

group :: Benchmark
group = bgroup "P60"
  [ subgroup "heightBalancedTreesWithNodes" Problem.heightBalancedTreesWithNodes
  , bgroup "Solutions"
    [ subgroup "heightBalancedTreesWithNodes"  Solution.heightBalancedTreesWithNodes ]
  ]

subgroup :: String -> (Int -> [Tree ()]) -> Benchmark
subgroup name heightBalancedTreesWithNodes = bgroup name
  [ bench "5"  $ nf heightBalancedTreesWithNodes 5
  , bench "15" $ nf heightBalancedTreesWithNodes 15
  ]
