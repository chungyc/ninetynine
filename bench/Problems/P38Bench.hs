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

subgroup :: String -> ([Integer]) -> Benchmark
subgroup name highlyTotientNumbers = bgroup name
  [ bench "take 5"  $ nf (\xs -> take 5 xs)  highlyTotientNumbers
  , bench "take 10" $ nf (\xs -> take 10 xs) highlyTotientNumbers
  ]
