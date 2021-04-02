{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P37Bench (group) where

import           Criterion
import qualified Problems.P37  as Problem
import qualified Solutions.P37 as Solution

group :: Benchmark
group = bgroup "P37"
  [ subgroup "totient'" Problem.totient'
  , bgroup "Solutions"
    [ subgroup "totient'" Solution.totient' ]
  ]

subgroup :: String -> (Integer -> Integer) -> Benchmark
subgroup name totient' = bgroup name
  [ bench  "10090" $ nf totient'  10090
  , bench  "84237" $ nf totient'  84237
  , bench "934579" $ nf totient' 934579
  ]
