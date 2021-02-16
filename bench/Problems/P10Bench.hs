module Problems.P10Bench (group) where

import           Criterion
import qualified Problems.P10  as Problem
import qualified Solutions.P10 as Solution

group :: Benchmark
group = bgroup "P10"
  [ subgroup "encode" Problem.encode
  , bgroup "Solutions"
    [ subgroup "encode" Solution.encode ]
  ]

subgroup :: String -> ([Int] -> [(Int, Int)]) -> Benchmark
subgroup name encode = bgroup name
  [ bench "[]"        $ nf encode []
  , bench "[1]"       $ nf encode [1]
  , bench "[1..10]"   $ nf encode [1..10]
  , bench "[1..100]"  $ nf encode [1..100]
  , bench "[1..1000]" $ nf encode [1..1000]
  , bench "replicate 10 1"   $ nf encode $ replicate 10 1
  , bench "replicate 100 1"  $ nf encode $ replicate 100 1
  , bench "replicate 1000 1" $ nf encode $ replicate 1000 1
  , bench "concat $ map (replicate 10) [1..10]"  $ nf encode $ concat $ map (replicate 10) [1..10]
  , bench "concat $ map (replicate 10) [1..100]" $ nf encode $ concat $ map (replicate 10) [1..100]
  ]
