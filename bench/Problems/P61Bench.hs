{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P61Bench (group) where

import           Criterion
import           Problems.BinaryTrees
import           Problems.P55
import qualified Problems.P61         as Problem
import qualified Solutions.P61        as Solution

group :: Benchmark
group = bgroup "P61"
  [ leavesGroup "leaves" Problem.leaves
  , internalsGroup "internals" Problem.internals
  , bgroup "Solutions"
    [ leavesGroup "leaves" Solution.leaves
    , internalsGroup "internals" Solution.internals
    ]
  ]

leavesGroup :: String -> (Tree () -> [()]) -> Benchmark
leavesGroup name leaves = bgroup name
  [ bench "tree size 5"  $ nf leaves (head $ completelyBalancedTrees 5)
  , bench "tree size 25" $ nf leaves (head $ completelyBalancedTrees 25)
  ]

internalsGroup :: String -> (Tree () -> [()]) -> Benchmark
internalsGroup name internals = bgroup name
  [ bench "tree size 5"  $ nf internals (head $ completelyBalancedTrees 5)
  , bench "tree size 25" $ nf internals (head $ completelyBalancedTrees 25)
  ]
