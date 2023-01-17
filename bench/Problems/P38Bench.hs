{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P38Bench (group) where

import           Criterion
import qualified Problems.P38  as Problem
import qualified Solutions.P38 as Solution

group :: Benchmark
group = bgroup "P38"
  [ subgroup "highlyTotientNumbers" Problem.highlyTotientNumbers
  , bgroup "Solutions"
    [ subgroup "highlyTotientNumbers" Solution.highlyTotientNumbers ]
  ]

subgroup :: String -> [Integer] -> Benchmark
subgroup name highlyTotientNumbers = bgroup name
  [ bench "take 5"  $ nf (take 5)  highlyTotientNumbers
  , bench "take 10" $ nf (take 10) highlyTotientNumbers
  ]
