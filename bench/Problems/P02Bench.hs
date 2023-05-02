{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P02Bench (group) where

import           Criterion
import qualified Problems.P02  as Problem
import qualified Solutions.P02 as Solution

group :: Benchmark
group = bgroup "P02"
  [ subgroup "myButLast" Problem.myButLast
  , bgroup "Solutions"
    [ subgroup "myButLast" Solution.myButLast ]
  ]

subgroup :: String -> ([Int] -> Maybe Int) -> Benchmark
subgroup name myButLast = bgroup name
  [ bench "[1..2]"    $ nf myButLast [1..2]
  , bench "[1..10]"   $ nf myButLast [1..10]
  , bench "[1..100]"  $ nf myButLast [1..100]
  , bench "[1..1000]" $ nf myButLast [1..1000]
  ]
