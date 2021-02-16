module Problems.P12Bench (group) where

import           Criterion
import           Problems.P11.Definitions
import qualified Problems.P12             as Problem
import qualified Solutions.P12            as Solution

group :: Benchmark
group = bgroup "P12"
  [ subgroup "decodeModified" Problem.decodeModified
  , bgroup "Solutions"
    [ subgroup "decodeModified"  Solution.decodeModified
    , subgroup "decodeModified'" Solution.decodeModified'
    ]
  ]

subgroup :: String -> ([Encoding Int] -> [Int]) -> Benchmark
subgroup name decodeModified = bgroup name
  [ bench "[]"         $ nf decodeModified []
  , bench "[Single 1]" $ nf decodeModified [Single 1]
  , bench "map Single [1..10]"   $ nf decodeModified $ map Single [1..10]
  , bench "map Single [1..100]"  $ nf decodeModified $ map Single [1..100]
  , bench "map Single [1..1000]" $ nf decodeModified $ map Single [1..1000]
  , bench "[Multiple 10 1]"   $ nf decodeModified [Multiple 10 1]
  , bench "[Multiple 100 1]"  $ nf decodeModified [Multiple 100 1]
  , bench "[Multiple 1000 1]" $ nf decodeModified [Multiple 1000 1]
  , bench "map (Multiple 10) [1..10]"  $ nf decodeModified $ map (Multiple 10) [1..10]
  , bench "map (Multiple 10) [1..100]" $ nf decodeModified $ map (Multiple 10) [1..10]
  ]
