{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P86Bench (group) where

import           Criterion
import           Data.List       (unfoldr)
import           Data.Maybe      (fromJust)
import qualified Data.Set        as Set
import           Problems.Graphs
import qualified Problems.P86    as Problem
import qualified Solutions.P86   as Solution
import           System.Random

group :: Benchmark
group = bgroup "P86"
  [ subgroup "colorGraph" Problem.colorGraph
  , bgroup "Solutions"
    [ subgroup "colorGraph" Solution.colorGraph ]
  ]

subgroup :: String -> (G -> [(Vertex,Int)]) -> Benchmark
subgroup name colorGraph = bgroup name
  [ bench "graph size 5"  $ nf colorGraph g5
  , bench "graph size 25" $ nf colorGraph g25
  ]
  where g5  = generate (mkStdGen 12) 5
        g25 = generate (mkStdGen 33) 25

-- | Generate an arbitrary but fixed graph with the given number of vertexes.
generate :: RandomGen r => r -> Int -> G
generate gen n =
  let vs = [1..n]
      ps = [Edge (u, v) | u <- vs, v <- vs, u < v]
      rs = unfoldr (Just . random) gen :: [Bool]
      es = map fst $ filter snd $ zip ps rs
  in fromJust $ toGraph (Set.fromList vs, Set.fromList es)
