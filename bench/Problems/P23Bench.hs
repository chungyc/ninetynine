{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P23Bench (group) where

import           Criterion
import qualified Problems.P23  as Problem
import qualified Solutions.P23 as Solution
import           System.Random

group :: Benchmark
group = bgroup "P23"
  [ subgroup "randomSelect" Problem.randomSelect
  , bgroup "Solutions"
    [ subgroup "randomSelect" Solution.randomSelect ]
  ]

subgroup :: String -> ([Int] -> Int -> StdGen -> ([Int], StdGen)) -> Benchmark
subgroup name randomSelect = bgroup name
  [ bench "[1..10] 5"     $  nf (fst . randomSelect [1..10]   5)   (mkStdGen 23)
  , bench "[1..1000] 100" $  nf (fst . randomSelect [1..1000] 100) (mkStdGen 24)
  ]
