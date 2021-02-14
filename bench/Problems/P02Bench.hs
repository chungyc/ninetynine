module Problems.P02Bench (group) where

import           Criterion
import           Problems.P02

group :: Benchmark
group = bgroup "P02" [
  bgroup "myButLast" [ bench "[1..2]" $ nf myButLast ([1..2] :: [Int])
                     , bench "[1..10]" $ nf myButLast ([1..10] :: [Int])
                     , bench "[1..100]" $ nf myButLast ([1..100] :: [Int])
                     , bench "[1..1000]" $ nf myButLast ([1..1000] :: [Int])
                     ]
  ]
