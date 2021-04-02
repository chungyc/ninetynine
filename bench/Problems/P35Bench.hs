{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P35Bench (group) where

import           Criterion
import qualified Problems.P35  as Problem
import qualified Solutions.P35 as Solution

group :: Benchmark
group = bgroup "P35"
  [ subgroup "primeFactors" Problem.primeFactors
  , bgroup "Solutions"
    [ subgroup "primeFactors" Solution.primeFactors ]
  ]

subgroup :: String -> (Integer -> [Integer]) -> Benchmark
subgroup name primeFactors = bgroup name
  [ bench  "84237" $ nf primeFactors  84237
  , bench "934579" $ nf primeFactors 934579
  ]
