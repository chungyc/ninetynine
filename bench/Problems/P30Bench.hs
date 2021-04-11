{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P30Bench (group) where

import           Criterion
import qualified Problems.P30  as Problem
import qualified Solutions.P30 as Solution

group :: Benchmark
group = bgroup "P30"
  [ subgroup "fibonacci'" Problem.fibonacci'
  , bgroup "Solutions"
    [ subgroup "fibonacci'" Solution.fibonacci' ]
  ]

subgroup :: String -> (Integer -> Integer) -> Benchmark
subgroup name fibonacci' = bgroup name
  [ bench "10"     $ nf fibonacci' 10
  , bench "100000" $ nf fibonacci' 100000
  ]
