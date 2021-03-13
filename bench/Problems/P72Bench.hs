module Problems.P72Bench (group) where

import           Criterion
import           Problems.MultiwayTrees
import qualified Problems.P72           as Problem
import qualified Solutions.P72          as Solution

group :: Benchmark
group = bgroup "P72"
  [ subgroup "postOrderSequence" Problem.postOrderSequence
  , bgroup "Solutions"
    [ subgroup "postOrderSequence"  Solution.postOrderSequence ]
  ]

subgroup :: String -> (MultiwayTree Char -> [Char]) -> Benchmark
subgroup name postOrderSequence = bgroup name
  [ bench "tree size 5101"  $ nf postOrderSequence
    (MultiwayTree 'a' $
     replicate 100 $ MultiwayTree 'b' . replicate 50 $ MultiwayTree 'c' []) ]
