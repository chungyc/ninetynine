{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P80Bench (group) where

import           Criterion
import           Data.List       (unfoldr)
import           Data.Maybe      (fromJust)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Problems.Graphs
import qualified Problems.P80    as Problem
import qualified Solutions.P80   as Solution
import           System.Random

group :: Benchmark
group = bgroup "P80"
  [ bgroup "from Lists"     $ fromLists (Problem.toAdjacency, Problem.toPaths, Problem.toG)
  , bgroup "from Adjacency" $ fromAdjacency (Problem.toLists, Problem.toPaths, Problem.toG)
  , bgroup "from Paths"     $ fromPaths (Problem.toLists, Problem.toAdjacency, Problem.toG)
  , bgroup "from G"         $ fromG (Problem.toLists, Problem.toAdjacency, Problem.toPaths)
  , bgroup "Solutions"
    [ bgroup "from Lists"     $ fromLists (Solution.toAdjacency, Solution.toPaths, Solution.toG)
    , bgroup "from Adjacency" $ fromAdjacency (Solution.toLists, Solution.toPaths, Solution.toG)
    , bgroup "from Paths"     $ fromPaths (Solution.toLists, Solution.toAdjacency, Solution.toG)
    , bgroup "from G"         $ fromG (Solution.toLists, Solution.toAdjacency, Solution.toPaths)
    ]
  ]

fromLists :: (Lists -> Adjacency, Lists -> Paths, Lists -> G) -> [Benchmark]
fromLists (toAdjacency, toPaths, toG) =
  let g100  = fromJust $ toGraph s100  :: Lists
      g1000 = fromJust $ toGraph s1000 :: Lists
      benchmarks convert = [ bench (size g100)  $ nf convert g100
                           , bench (size g1000) $ nf convert g1000 ]
  in [ bgroup "toLists" $ benchmarks toAdjacency
     , bgroup "toPaths" $ benchmarks toPaths
     , bgroup "toG"     $ benchmarks toG
     ]

fromAdjacency :: (Adjacency -> Lists, Adjacency -> Paths, Adjacency -> G) -> [Benchmark]
fromAdjacency (toLists, toPaths, toG) =
  let g100  = fromJust $ toGraph s100  :: Adjacency
      g1000 = fromJust $ toGraph s1000 :: Adjacency
      benchmarks convert = [ bench (size g100)  $ nf convert g100
                           , bench (size g1000) $ nf convert g1000 ]
  in [ bgroup "toLists" $ benchmarks toLists
     , bgroup "toPaths" $ benchmarks toPaths
     , bgroup "toG"     $ benchmarks toG
     ]

fromPaths :: (Paths -> Lists, Paths -> Adjacency, Paths -> G) -> [Benchmark]
fromPaths (toLists, toAdjacency, toG) =
  let g100  = fromJust $ toGraph s100  :: Paths
      g1000 = fromJust $ toGraph s1000 :: Paths
      benchmarks convert = [ bench (size g100)  $ nf convert g100
                           , bench (size g1000) $ nf convert g1000 ]
  in [ bgroup "toLists"     $ benchmarks toLists
     , bgroup "toAdjacency" $ benchmarks toAdjacency
     , bgroup "toG"         $ benchmarks toG
     ]

fromG :: (G -> Lists, G -> Adjacency, G -> Paths) -> [Benchmark]
fromG (toLists, toAdjacency, toPaths) =
  let g100  = fromJust $ toGraph s100  :: G
      g1000 = fromJust $ toGraph s1000 :: G
      benchmarks convert = [ bench (size g100)  $ nf convert g100
                           , bench (size g1000) $ nf convert g1000 ]
  in [ bgroup "toLists"     $ benchmarks toLists
     , bgroup "toAdjacency" $ benchmarks toAdjacency
     , bgroup "toPaths"     $ benchmarks toPaths
     ]

s100 :: (Set Vertex, Set Edge)
s100 = generate (mkStdGen 12) 100

s1000 :: (Set Vertex, Set Edge)
s1000 = generate (mkStdGen 123) 1000

-- | Generate an arbitrary but fixed graph with the given number of vertexes.
generate :: RandomGen r => r -> Int -> (Set Vertex, Set Edge)
generate gen n =
  let vs = [1..n]
      ps = [Edge (u, v) | u <- vs, v <- vs, u < v]
      rs = unfoldr (Just . random) gen :: [Bool]
      es = map fst $ filter snd $ zip ps rs
  in (Set.fromList vs, Set.fromList es)

size :: Graph g => g -> String
size g = "graph size " ++ show (Set.size $ vertexes g, Set.size $ edges g)
