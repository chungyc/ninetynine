{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.TutorialBench (group) where

import           Criterion
import qualified Problems.Tutorial  as Problem
import qualified Solutions.Tutorial as Solution

group :: Benchmark
group = bgroup "Tutorial"
  [ subgroup "sumNumbers" Problem.sumNumbers
  , bgroup "Solutions"
    [ subgroup "sumNumbers" Solution.sumNumbers
    , subgroup "sumNumbers'" Solution.sumNumbers'
    , subgroup "sumNumbers''" Solution.sumNumbers''
    ]
  ]

subgroup :: String -> (Integer -> Integer) -> Benchmark
subgroup name sumNumbers = bgroup name
  [ bench "1"       $ nf sumNumbers 1
  , bench "10"      $ nf sumNumbers 10
  , bench "100"     $ nf sumNumbers 100
  , bench "1000000" $ nf sumNumbers 1000000
  ]
