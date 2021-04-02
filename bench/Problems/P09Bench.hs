{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P09Bench (group) where

import           Criterion
import qualified Problems.P09  as Problem
import qualified Solutions.P09 as Solution

group :: Benchmark
group = bgroup "P09"
  [ subgroup "pack" Problem.pack
  , bgroup "Solutions"
    [ subgroup "pack"  Solution.pack
    , subgroup "pack'" Solution.pack' ]
  ]

subgroup :: String -> ([Int] -> [[Int]]) -> Benchmark
subgroup name pack = bgroup name
  [ bench "[]"        $ nf pack []
  , bench "[1]"       $ nf pack [1]
  , bench "[1..10]"   $ nf pack [1..10]
  , bench "[1..100]"  $ nf pack [1..100]
  , bench "[1..1000]" $ nf pack [1..1000]
  , bench "replicate 10 1"   $ nf pack $ replicate 10 1
  , bench "replicate 100 1"  $ nf pack $ replicate 100 1
  , bench "replicate 1000 1" $ nf pack $ replicate 1000 1
  , bench "concat $ map (replicate 10) [1..10]"  $ nf pack $ concat $ map (replicate 10) [1..10]
  , bench "concat $ map (replicate 10) [1..100]" $ nf pack $ concat $ map (replicate 10) [1..100]
  ]
