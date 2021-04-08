{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P96Bench (group) where

import           Criterion
import qualified Problems.P96  as Problem
import qualified Solutions.P96 as Solution

group :: Benchmark
group = bgroup "P96"
  [ subgroup "isIdentifier" Problem.isIdentifier
  , bgroup "Solutions"
    [ subgroup "isIdentifier" Solution.isIdentifier ]
  ]

subgroup :: String -> (String -> Bool) -> Benchmark
subgroup name isIdentifier = bgroup name
  [ bench "\"Fibonacci_sequence_is_1_1_2_3_5_8_13_21_ad_infinitum\"" $
    nf isIdentifier "Fibonacci_sequence_is_1_1_2_3_5_8_13_21_ad_infinitum"
  ]
