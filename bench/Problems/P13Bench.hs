{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P13Bench (group) where

import           Criterion
import           Problems.Lists
import qualified Problems.P13   as Problem
import qualified Solutions.P13  as Solution

group :: Benchmark
group = bgroup "P13"
  [ subgroup "encodeDirect" Problem.encodeDirect
  , bgroup "Solutions"
    [ subgroup "encodeDirect" Solution.encodeDirect ]
  ]

subgroup :: String -> ([Int] -> [Encoding Int]) -> Benchmark
subgroup name encodeDirect = bgroup name
  [ bench "[]"        $ nf encodeDirect []
  , bench "[1]"       $ nf encodeDirect [1]
  , bench "[1..10]"   $ nf encodeDirect [1..10]
  , bench "[1..100]"  $ nf encodeDirect [1..100]
  , bench "[1..1000]" $ nf encodeDirect [1..1000]
  , bench "replicate 10 1" $ nf encodeDirect   $ replicate 10 1
  , bench "replicate 100 1" $ nf encodeDirect  $ replicate 100 1
  , bench "replicate 1000 1" $ nf encodeDirect $ replicate 1000 1
  , bench "concatMap (replicate 10) [1..10]"  $ nf encodeDirect $ concatMap (replicate 10) [1..10]
  , bench "concatMap (replicate 10) [1..100]" $ nf encodeDirect $ concatMap (replicate 10) [1..100]
  ]
