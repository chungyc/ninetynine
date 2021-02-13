module Problems.P01Bench (group) where

import           Criterion
import           Problems.P01

group :: Benchmark
group = bgroup "P01" [
  bgroup "myLast" [ bench "1"     $ nf myLast ([1] :: [Int])
                  , bench "10"    $ nf myLast ([1..10] :: [Int])
                  , bench "100"  $ nf myLast ([1..100] :: [Int])
                  , bench "1000" $ nf myLast ([1..1000] :: [Int])
                  ]
  ]
