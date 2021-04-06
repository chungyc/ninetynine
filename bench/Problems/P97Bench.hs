{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P97Bench (group) where

import           Criterion
import           Problems.P97  (sudokuPuzzle)
import qualified Problems.P97  as Problem
import qualified Solutions.P97 as Solution

group :: Benchmark
group = bgroup "P97"
  [ bench "sudoku sudokuPuzzle" $ nf Problem.sudoku sudokuPuzzle
  , bench "sudoku blankPuzzle" $ nf Problem.sudoku blankPuzzle
  , bgroup "Solutions"
    [ bench "sudoku sudokuPuzzle" $ nf Solution.sudoku sudokuPuzzle
    , bench "sudoku blankPuzzle" $ nf Solution.sudoku blankPuzzle
    ]
  ]

blankPuzzle :: [[Int]]
blankPuzzle = replicate 9 $ replicate 9 0
