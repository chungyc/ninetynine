module Problems.P24Bench (group) where

import           Criterion
import qualified Problems.P24  as Problem
import qualified Solutions.P24 as Solution
import           System.Random

group :: Benchmark
group = bgroup "P24"
  [ subgroup "randomDraw" Problem.randomDraw
  , bgroup "Solutions"
    [ subgroup "randomDraw" Solution.randomDraw ]
  ]

subgroup :: String -> (Int -> Int -> StdGen -> ([Int], StdGen)) -> Benchmark
subgroup name randomDraw = bgroup name
  [ bench "5 10"     $  nf (fst . randomDraw 5   10)   (mkStdGen 23)
  , bench "100 1000" $  nf (fst . randomDraw 100 1000) (mkStdGen 24)
  ]
