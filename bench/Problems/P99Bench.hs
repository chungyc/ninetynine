{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P99Bench (group) where

import           Criterion
import           Problems.Crosswords
import qualified Problems.P99        as Problem
import qualified Solutions.P99       as Solution

group :: Benchmark
group = bgroup "P99"
  [ subgroup "solveCrossword" Problem.solveCrossword
  , bgroup "Solutions"
    [ subgroup "solveCrossword" Solution.solveCrossword ]
  ]

subgroup :: String -> (Crossword -> Maybe [[Maybe Char]]) -> Benchmark
subgroup name solveCrossword = bgroup name
  [ bench "5x6"   $ nf solveCrossword Problem.crosswordPuzzle
  , bench "25x25" $ nf solveCrossword Problem.crosswordPuzzle'
  ]
