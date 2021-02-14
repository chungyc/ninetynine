module Problems.P03Bench (group) where

import           Criterion
import           Problems.P03

group :: Benchmark
group = bgroup "P03" [
  bgroup "elementAt" [
      bgroup "[1]"   [ bench "1" $ nf (elementAt ([1] :: [Int])) 1 ],
      bgroup "[1..100]" [ bench "1" $ nf (elementAt ([1..100] :: [Int])) 1
                        , bench "50" $ nf (elementAt ([1..100] :: [Int])) 50
                        , bench "100" $ nf (elementAt ([1..100] :: [Int])) 100
                        ]
      ]
  ]
