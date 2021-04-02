{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P36Bench (group) where

import           Criterion
import qualified Problems.P36  as Problem
import qualified Solutions.P36 as Solution

group :: Benchmark
group = bgroup "P36"
  [ subgroup "primeFactorsMultiplicity" Problem.primeFactorsMultiplicity
  , bgroup "Solutions"
    [ subgroup "primeFactorsMultiplicity" Solution.primeFactorsMultiplicity ]
  ]

subgroup :: String -> (Integer -> [(Integer, Integer)]) -> Benchmark
subgroup name primeFactorsMultiplicity = bgroup name
  [ bench  "84237" $ nf primeFactorsMultiplicity  84237
  , bench "934579" $ nf primeFactorsMultiplicity 934579
  ]
