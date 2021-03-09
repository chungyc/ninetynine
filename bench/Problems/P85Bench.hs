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
    [ subgroup "isomorphic"  Solution.isomorphic
    , subgroup "isomorphic'" Solution.isomorphic'
    ]
  ]

subgroup :: String -> (G -> G -> Bool) -> Benchmark
subgroup name isomorphic = bgroup name
  [ bench "graph size 5" $ nf (isomorphic g5) g5'
  , bench "graph size 8" $ nf (isomorphic g8) g8'
  ]
  where g5  = generate (mkStdGen 12) 5
        g5' = generate (mkStdGen 13) 5
        g8  = generate (mkStdGen 33) 8
        g8' = generate (mkStdGen 34) 8

-- | Generate an arbitrary but fixed graph with the given number of vertexes.
generate :: RandomGen r => r -> Int -> G
generate gen n =
  let vs = [1..n]
      ps = [Edge (u, v) | u <- vs, v <- vs, u < v]
      rs = unfoldr (Just . random) gen :: [Bool]
      es = map fst $ filter snd $ zip ps rs
  in fromJust $ toGraph (Set.fromList vs, Set.fromList es)
