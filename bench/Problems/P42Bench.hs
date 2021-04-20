{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P42Bench (group) where

import           Criterion
import qualified Problems.P42  as Problem
import qualified Solutions.P42 as Solution

group :: Benchmark
group = bgroup "P42"
  [ subgroup "multiplicativeInverse" Problem.multiplicativeInverse
  , bgroup "Solutions"
    [ subgroup "multiplicativeInverse" Solution.multiplicativeInverse ]
  ]

subgroup :: String -> (Integer -> Integer -> Maybe Integer) -> Benchmark
subgroup name multiplicativeInverse = bgroup name
  [ bench "55 547"     $ nf (multiplicativeInverse 100)  547
  , bench "55 1299721" $ nf (multiplicativeInverse 1000) 1299721
  ]
