{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P92Bench (group) where

import           Criterion
import           Data.Map        (Map)
import           Problems.Graphs
import           Problems.P92    (tree92, tree92')
import qualified Problems.P92    as Problem
import qualified Solutions.P92   as Solution

group :: Benchmark
group = bgroup "P92"
  [ subgroup "gracefulTree" Problem.gracefulTree
  , bgroup "Solutions"
    [ subgroup     "gracefulTree"  Solution.gracefulTree
    , slowsubgroup "gracefulTree'" Solution.gracefulTree'
    ]
  ]

subgroup :: String -> (G -> Maybe (Map Vertex Int)) -> Benchmark
subgroup name gracefulTree = bgroup name
  [ bench "tree92"  $ nf gracefulTree tree92
  , bench "tree92'" $ nf gracefulTree tree92'
  ]

-- | Benchmarking implementations which are too slow to run with tree92'.
slowsubgroup :: String -> (G -> Maybe (Map Vertex Int)) -> Benchmark
slowsubgroup name gracefulTree = bgroup name
  [ bench "tree92" $ nf gracefulTree tree92 ]
