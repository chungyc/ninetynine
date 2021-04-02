{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P93Bench (group) where

import           Criterion
import qualified Problems.P93  as Problem
import qualified Solutions.P93 as Solution

group :: Benchmark
group = bgroup "P93"
  [ subgroup "arithmeticPuzzle" Problem.arithmeticPuzzle
  , bgroup "Solutions"
    [ subgroup "arithmeticPuzzle"  Solution.arithmeticPuzzle ]
  ]

subgroup :: String -> ([Integer] -> [String]) -> Benchmark
subgroup name arithmeticPuzzle = bgroup name
  [ bench "[2,3,5,7,11]"     $ nf arithmeticPuzzle [2,3,5,7,11]
  , bench "[4,3,10,7,2,4,6]" $ nf arithmeticPuzzle [4,3,10,7,2,4,6]
  ]
