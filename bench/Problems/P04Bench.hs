module Problems.P04Bench (group) where

import           Criterion
import           Problems.P04

group :: Benchmark
group = bgroup "P04" [
  bgroup "myLength" [ bench "0"    $ nf myLength ([] :: [Int])
                    , bench "1"    $ nf myLength ([1] :: [Int])
                    , bench "10"   $ nf myLength ([1..10] :: [Int])
                    , bench "100"  $ nf myLength ([1..100] :: [Int])
                    , bench "1000" $ nf myLength ([1..1000] :: [Int])
                    ]
  ]
