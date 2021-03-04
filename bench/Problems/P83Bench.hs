module Problems.P83Bench (group) where

import           Control.DeepSeq
import           Criterion
import           Data.List       (unfoldr)
import           Data.Maybe      (fromJust)
import qualified Data.Set        as Set
import           Problems.Graphs
import qualified Problems.P83    as Problem
import qualified Solutions.P83   as Solution
import           System.Random

group :: Benchmark
group = bgroup "P83"
  [ subgroup "spanningTrees" Problem.spanningTrees
  , subgroup "isTree" Problem.isTree
  , subgroup "isConnected" Problem.isConnected
  , bgroup "Solutions"
    [ subgroup "spanningTrees" Solution.spanningTrees
    , subgroup "isTree" Solution.isTree
    , subgroup "isConnected" Solution.isConnected
    ]
  ]

subgroup :: NFData a => String -> (G -> a) -> Benchmark
subgroup name f = bgroup name
  [ bench (size g5)  $ nf f g5
  , bench (size g10) $ nf f g10
  ]
  where g5 = generate (mkStdGen 12) 5
        g10 = generate (mkStdGen 23) 10

-- | Generate an arbitrary but fixed graph with the given number of vertexes.
generate :: RandomGen r => r -> Int -> G
generate gen n =
  let vs = [1..n]
      ps = [Edge (u, v) | u <- vs, v <- vs, u < v]
      rs = unfoldr (Just . random) gen :: [Bool]
      es = map fst $ filter snd $ zip ps rs
  in fromJust $ toGraph (Set.fromList vs, Set.fromList es)

size :: Graph g => g -> String
size g = "graph size " ++ show (Set.size $ vertexes g, Set.size $ edges g)
