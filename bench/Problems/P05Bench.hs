{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P05Bench (group) where

import           Criterion
import qualified Problems.P05  as Problem
import qualified Solutions.P05 as Solution

group :: Benchmark
group = bgroup "P05"
  [ subgroup "myReverse" Problem.myReverse
  , bgroup "Solutions"
    [ subgroup "myReverse" Solution.myReverse ]
  ]

subgroup :: String -> ([Int] -> [Int]) -> Benchmark
subgroup name myReverse = bgroup name
  [ bench "[]"        $ nf myReverse []
  , bench "[1]"       $ nf myReverse [1]
  , bench "[1..10]"   $ nf myReverse [1..10]
  , bench "[1..100]"  $ nf myReverse [1..100]
  , bench "[1..1000]" $ nf myReverse [1..1000]
  ]
