{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P03Bench (group) where

import           Criterion
import qualified Problems.P03  as Problem
import qualified Solutions.P03 as Solution

group :: Benchmark
group = bgroup "P03"
  [ subgroup "elementAt" Problem.elementAt
  , bgroup "Solutions"
    [ subgroup "elementAt" Solution.elementAt ]
  ]

subgroup :: String -> ([Int] -> Int -> Maybe Int) -> Benchmark
subgroup name elementAt = bgroup name
  [ bgroup "[1]"      [ bench "1"   $ nf (elementAt [1]) 1 ]
  , bgroup "[1..100]" [ bench "1"   $ nf (elementAt [1..100]) 1
                      , bench "50"  $ nf (elementAt [1..100]) 50
                      , bench "100" $ nf (elementAt [1..100]) 100
                      ]
  ]
