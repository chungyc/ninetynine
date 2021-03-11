module Problems.P71Bench (group) where

import           Criterion
import           Problems.MultiwayTrees
import qualified Problems.P71           as Problem
import qualified Solutions.P71          as Solution

group :: Benchmark
group = bgroup "P71"
  [ subgroup "internalPathLength" Problem.internalPathLength
  , bgroup "Solutions"
    [ subgroup "internalPathLength"  Solution.internalPathLength ]
  ]

subgroup :: String -> (MultiwayTree Char -> Int) -> Benchmark
subgroup name internalPathLength = bgroup name
  [ bench "tree size 301"  $ nf internalPathLength
    (MultiwayTree 'a' $
     replicate 100 $ MultiwayTree 'b' [MultiwayTree 'c' [],
                                       MultiwayTree 'd' []]) ]
