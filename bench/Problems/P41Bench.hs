{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P41Bench (group) where

import           Criterion
import qualified Problems.P41  as Problem
import qualified Solutions.P41 as Solution

group :: Benchmark
group = bgroup "P41"
  [ subgroup "goldbachList" Problem.goldbachList
  , bgroup "Solutions"
    [ subgroup "goldbachList" Solution.goldbachList ]
  ]

subgroup :: String -> (Integer -> Integer -> [(Integer,Integer)]) -> Benchmark
subgroup name goldbachList = bgroup name
  [ bench "100 1000"   $ nf (goldbachList 100)  1000
  , bench "1000 10000" $ nf (goldbachList 1000) 10000
  ]
