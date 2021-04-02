{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P08Bench (group) where

import           Criterion
import qualified Problems.P08  as Problem
import qualified Solutions.P08 as Solution

group :: Benchmark
group = bgroup "P08"
  [ subgroup "compress" Problem.compress
  , bgroup "Solutions"
    [ subgroup "compress" Solution.compress ]
  ]

subgroup :: String -> ([Int] -> [Int]) -> Benchmark
subgroup name compress = bgroup name
  [ bench "[]"        $ nf compress []
  , bench "[1..10]"   $ nf compress [1..10]
  , bench "[1..100]"  $ nf compress [1..100]
  , bench "[1..1000]" $ nf compress [1..1000]
  , bench "replicate 10 1"   $ nf compress $ replicate 10 1
  , bench "replicate 100 1"  $ nf compress $ replicate 100 1
  , bench "replicate 1000 1" $ nf compress $ replicate 1000 1
  , bench "concat $ map (replicate 10) [1..10]" $
    nf compress $ concat $ map (replicate 10) [1..10]
  , bench "concat $ map (replicate 10) [1..100]" $
    nf compress $ concat $ map (replicate 10) [1..100]
  ]
