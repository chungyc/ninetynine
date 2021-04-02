{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P33Bench (group) where

import           Criterion
import qualified Problems.P33  as Problem
import qualified Solutions.P33 as Solution

group :: Benchmark
group = bgroup "P33"
  [ subgroup "coprime" Problem.coprime
  , bgroup "Solutions"
    [ subgroup "coprime" Solution.coprime ]
  ]

subgroup :: String -> (Integer -> Integer -> Bool) -> Benchmark
subgroup name coprime = bgroup name
  [ bench "  84237  66323" $ nf (coprime   84237)  66323
  , bench "9345792  34884" $ nf (coprime 9345792)  34884
  , bench " 129923 902039" $ nf (coprime  129923) 902039
  ]
