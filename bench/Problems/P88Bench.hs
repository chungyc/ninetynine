{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P88Bench (group) where

import           Criterion
import           Data.List       (unfoldr)
import           Data.Maybe      (fromJust)
import qualified Data.Set        as Set
import           Problems.Graphs
import qualified Problems.P88    as Problem
import qualified Solutions.P88   as Solution
import           System.Random

group :: Benchmark
group = bgroup "P88"
  [ subgroup "connectedComponents" Problem.connectedComponents
  , bgroup "Solutions"
    [ subgroup "connectedComponents" Solution.connectedComponents ]
  ]

subgroup :: String -> (G -> [[Vertex]]) -> Benchmark
subgroup name connectedComponents = bgroup name
  [ bench "graph size 5"   $ nf connectedComponents g5
  , bench "graph size 100" $ nf connectedComponents g100
  ]
  where g5   = generate (mkStdGen 12) 5
        g100 = generate (mkStdGen 33) 100

-- | Generate an arbitrary but fixed graph with the given number of vertexes.
generate :: RandomGen r => r -> Int -> G
generate gen n =
  let vs = [1..n]
      ps = [Edge (u, v) | u <- vs, v <- vs, u < v]
      rs = unfoldr (Just . random) gen :: [Bool]
      es = map fst $ filter snd $ zip ps rs
  in fromJust $ toGraph (Set.fromList vs, Set.fromList es)
