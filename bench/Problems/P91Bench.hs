{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P91Bench (group) where

import           Criterion
import qualified Problems.P91  as Problem
import qualified Solutions.P91 as Solution

group :: Benchmark
group = bgroup "P91"
  [ knightsTourGroup "knightsTour" Problem.knightsTour
  , closedKnightsTourGroup "closedKnightsTour" Problem.closedKnightsTour
  , bgroup "Solutions"
    [ knightsTourGroup "knightsTour" Solution.knightsTour
    , closedKnightsTourGroup "closedKnightsTour" Solution.closedKnightsTour
    ]
  ]

knightsTourGroup :: String -> (Int -> (Int,Int) -> Maybe [(Int,Int)]) -> Benchmark
knightsTourGroup name knightsTour = bgroup name
  [ bench "6 (3,5)" $ nf (knightsTour 6) (3,5)
  , bench "8 (1,1)" $ nf (knightsTour 8) (1,1)
  ]

closedKnightsTourGroup :: String -> (Int -> Maybe [(Int,Int)]) -> Benchmark
closedKnightsTourGroup name closedKnightsTour = bgroup name
  [ bench "6" $ nf closedKnightsTour 6 ]
