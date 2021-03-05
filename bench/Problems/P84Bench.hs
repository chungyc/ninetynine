module Problems.P84Bench (group) where

import           Criterion
import           Data.List       (unfoldr)
import           Data.Map.Lazy   (Map)
import qualified Data.Map.Lazy   as Map
import           Data.Maybe      (fromJust)
import qualified Data.Set        as Set
import           Problems.Graphs
import qualified Problems.P84    as Problem
import qualified Solutions.P84   as Solution
import           System.Random

group :: Benchmark
group = bgroup "P84"
  [ subgroup "minimumSpanningTree" Problem.minimumSpanningTree
  , bgroup "Solutions"
    [ subgroup "minimumSpanningTrees" Solution.minimumSpanningTree ]
  ]

subgroup :: String -> (G -> Map Edge Int -> G) -> Benchmark
subgroup name minimumSpanningTree = bgroup name
  [ bench (size g5)  $ nf (minimumSpanningTree g5)  w5
  , bench (size g10) $ nf (minimumSpanningTree g10) w10
  ]
  where (g5,  w5)  = generate (mkStdGen 12) 5
        (g10, w10) = generate (mkStdGen 23) 10

-- | Generate an arbitrary but fixed graph with the given number of vertexes.
generate :: RandomGen r => r -> Int -> (G, Map Edge Int)
generate gen n =
  let (gen', gen'') = split gen
      vs = [1..n]
      ps = [Edge (u, v) | u <- vs, v <- vs, u < v]
      rs = unfoldr (Just . random) gen' :: [Bool]
      rs' = unfoldr (Just . random) gen'' :: [Int]
      es = map fst $ filter snd $ zip ps rs
      g = fromJust $ toGraph (Set.fromList vs, Set.fromList es)
      ws = Map.fromList $ zip (Set.toList $ edges g) rs'
  in (g, ws)

size :: Graph g => g -> String
size g = "graph size " ++ show (Set.size $ vertexes g, Set.size $ edges g)
