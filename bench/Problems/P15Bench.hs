module Problems.P15Bench (group) where

import           Criterion
import           Problems.P15

group :: Benchmark
group = bgroup "P15" [
  bgroup "repli" [
      bgroup "[1..10]" [ bench "1" $ nf (repli [1..10 :: Int]) 1
                       , bench "10" $ nf (repli [1..10 :: Int]) 10
                       , bench "100" $ nf (repli [1..10 :: Int]) 100
                       , bench "1000" $ nf (repli [1..10 :: Int]) 1000
                       ],
      bgroup "[1..1000]" [ bench "1" $  nf (repli [1..1000 :: Int]) 1
                         , bench "10" $  nf (repli [1..1000 :: Int]) 10
                         , bench "100" $  nf (repli [1..1000 :: Int]) 100
                         ]
      ]
  ]
