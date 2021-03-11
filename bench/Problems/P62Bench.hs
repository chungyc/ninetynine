module Problems.P62Bench (group) where

import           Criterion
import           Problems.BinaryTrees
import qualified Problems.P62         as Problem
import qualified Solutions.P62        as Solution
import           System.Random

group :: Benchmark
group = bgroup "P62"
  [ subgroup "atLevel" Problem.atLevel
  , bgroup "Solutions"
    [ subgroup "atLevel"  Solution.atLevel ]
  ]

subgroup :: String -> (Tree () -> Int -> [()]) -> Benchmark
subgroup name atLevel = bgroup name
  [ bench "100 50"     $ nf (atLevel $ generate (mkStdGen 8589345) 100) 50
  , bench "10000 7500" $ nf (atLevel $ generate (mkStdGen 34734) 10000) 7500
  ]

generate :: RandomGen g => g -> Int -> Tree ()
generate _ 0 = Empty
generate gen n = Branch () left right
  where (m, gen') = randomR (0, n-1) gen
        (gen'', gen''') = split gen'
        left = generate gen'' m
        right = generate gen''' (n-m-1)
