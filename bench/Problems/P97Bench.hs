module Problems.P97Bench (group) where

import           Criterion
import           Problems.P97  (sudokupuzzle)
import qualified Problems.P97  as Problem
import qualified Solutions.P97 as Solution

group :: Benchmark
group = bgroup "P97"
  [ bench "sudoku sudokupuzzle" $ nf Problem.sudoku sudokupuzzle
  , bench "sudoku blankpuzzle" $ nf Problem.sudoku blankpuzzle
  , bgroup "Solutions"
    [ bench "sudoku sudokupuzzle" $ nf Solution.sudoku sudokupuzzle
    , bench "sudoku blankpuzzle" $ nf Solution.sudoku blankpuzzle
    ]
  ]

blankpuzzle :: [[Int]]
blankpuzzle = replicate 9 $ replicate 9 0
