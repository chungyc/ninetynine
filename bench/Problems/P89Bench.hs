{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P89Bench (group) where

import           Criterion
import           Data.List       (unfoldr)
import           Data.Maybe      (fromJust)
import qualified Data.Set        as Set
import           Problems.Graphs
import qualified Problems.P89    as Problem
import qualified Solutions.P89   as Solution
import           System.Random

group :: Benchmark
group = bgroup "P89"
  [ subgroup "bipartite" Problem.bipartite
  , bgroup "Solutions"
    [ subgroup "bipartite" Solution.bipartite ]
  ]

subgroup :: String -> (G -> Bool) -> Benchmark
subgroup name bipartite = bgroup name
  [ bench "graph size 5"   $ nf bipartite g5
  , bench "graph size 100" $ nf bipartite g100
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
