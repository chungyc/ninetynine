{-|
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P44Bench (group) where

import           Criterion
import           Data.Complex
import qualified Problems.P44  as Problem
import qualified Solutions.P44 as Solution

group :: Benchmark
group = bgroup "P44"
  [ subgroup "isGaussianPrime" Problem.isGaussianPrime
  , bgroup "Solutions"
    [ subgroup "isGaussianPrime" Solution.isGaussianPrime ]
  ]

subgroup :: String -> (Complex Integer -> Bool) -> Benchmark
subgroup name isGaussianPrime =
  bgroup name
  [ bench "12831:+3744" $ nf isGaussianPrime (12831:+3744)
  , bench "9283489241:+12388287444" $ nf isGaussianPrime (123717473:+757577744)
  ]
