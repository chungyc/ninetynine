{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P15Bench (group) where

import           Criterion
import qualified Problems.P15  as Problem
import qualified Solutions.P15 as Solution

group :: Benchmark
group = bgroup "P15"
  [ subgroup "repli" Problem.repli
  , bgroup "Solutions"
    [ subgroup "repli" Solution.repli ]
  ]

subgroup :: String -> ([Int] -> Int -> [Int]) -> Benchmark
subgroup name repli = bgroup name
  [ bgroup "[1..10]" [ bench "1"    $ nf (repli [1..10]) 1
                     , bench "10"   $ nf (repli [1..10]) 10
                     , bench "100"  $ nf (repli [1..10]) 100
                     , bench "1000" $ nf (repli [1..10]) 1000
                     ]
  , bgroup "[1..1000]" [ bench "1"   $  nf (repli [1..1000]) 1
                       , bench "10"  $  nf (repli [1..1000]) 10
                       , bench "100" $  nf (repli [1..1000]) 100
                       ]
  ]
