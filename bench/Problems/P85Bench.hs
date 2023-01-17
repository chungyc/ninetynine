{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P85Bench (group) where

import           Criterion
import           Data.List       (unfoldr)
import           Data.Maybe      (fromJust)
import qualified Data.Set        as Set
import           Problems.Graphs
import qualified Problems.P85    as Problem
import qualified Solutions.P85   as Solution
import           System.Random

group :: Benchmark
group = bgroup "P85"
  [ subgroup "isomorphic" Problem.isomorphic
  , bgroup "Solutions"
    [ subgroup "isomorphic"   Solution.isomorphic
    , subgroup "isomorphic'"  Solution.isomorphic'
    , subgroup "isomorphic''" Solution.isomorphic''
    ]
  ]

subgroup :: String -> (G -> G -> Bool) -> Benchmark
subgroup name isomorphic = bgroup name
  [ bench "graph size 5" $ nf (isomorphic g5) g5'
  , bench "graph size 8" $ nf (isomorphic g8) g8'
  , bench "4-regular graph, size 8" $ nf (isomorphic rg8) rg8'
  ]
  where g5  = generate (mkStdGen 12) 5
        g5' = generate (mkStdGen 13) 5
        g8  = generate (mkStdGen 33) 8
        g8' = generate (mkStdGen 34) 8
        rg8  = fromJust $ toGraph
               (Set.fromList [1..8],
                Set.fromList $ map Edge [ (1,2), (1,3), (1,4), (1,5)
                                        , (2,3), (2,4), (2,5), (3,6)
                                        , (3,7), (4,6), (4,8), (5,7)
                                        , (5,8), (6,7), (6,8), (7,8)
                                        ])
        rg8' = fromJust $ toGraph
               (Set.fromList [1..8],
                Set.fromList $ map Edge [ (8,7), (8,6), (8,5), (8,4)
                                        , (7,6), (7,5), (7,4), (6,3)
                                        , (6,2), (5,3), (5,1), (4,2)
                                        , (4,1), (3,2), (3,1), (2,1)
                                        ])

-- | Generate an arbitrary but fixed graph with the given number of vertexes.
generate :: RandomGen r => r -> Int -> G
generate gen n =
  let vs = [1..n]
      ps = [Edge (u, v) | u <- vs, v <- vs, u < v]
      rs = unfoldr (Just . random) gen :: [Bool]
      es = map fst $ filter snd $ zip ps rs
  in fromJust $ toGraph (Set.fromList vs, Set.fromList es)
