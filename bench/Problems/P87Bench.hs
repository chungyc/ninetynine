{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P87Bench (group) where

import           Criterion
import           Data.List       (unfoldr)
import           Data.Maybe      (fromJust)
import qualified Data.Set        as Set
import           Problems.Graphs
import qualified Problems.P87    as Problem
import qualified Solutions.P87   as Solution
import           System.Random

group :: Benchmark
group = bgroup "P87"
  [ subgroup "depthFirst" Problem.depthFirst
  , bgroup "Solutions"
    [ subgroup "depthFirst" Solution.depthFirst ]
  ]

subgroup :: String -> (G -> Vertex -> [Vertex]) -> Benchmark
subgroup name depthFirst = bgroup name
  [ bench "graph size 5"   $ nf (depthFirst g5)   $ Set.findMin $ vertexes g5
  , bench "graph size 100" $ nf (depthFirst g100) $ Set.findMin $ vertexes g100
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
