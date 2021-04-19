{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P47Bench (group) where

import           Criterion
import qualified Problems.P47  as Problem
import qualified Solutions.P47 as Solution

group :: Benchmark
group = bgroup "P47"
  [ bench (evaluateName "evaluateCircuit") $ nf (Problem.evaluateCircuit circuit True) False
  , bench "buildCircuit (==)" $ nf Problem.buildCircuit (==)
  , bgroup "Solutions"
    [ bench (evaluateName "evaluateCircuit") $ nf (Solution.evaluateCircuit circuit True) False
    , bench "buildCircuit (==)" $ nf Solution.buildCircuit (==)
    ]
  ]
  where evaluateName name = name ++ " " ++ show circuit ++ " True False"

circuit :: [(Int,Int)]
circuit = [(-1,-1),(-1,-2),(1,-2),(1,3),(2,4),(2,5)]
