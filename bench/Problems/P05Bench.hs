module Problems.P05Bench (group) where

import           Criterion
import           Problems.P05

group :: Benchmark
group = bgroup "P05" [
  bgroup "myReverse" [ bench "[]" $ nf myReverse ([] :: [Int])
                     , bench "[1]" $ nf myReverse [1 :: Int]
                     , bench "[1..10]" $ nf myReverse [1..10 :: Int]
                     , bench "[1..100]" $ nf myReverse [1..100 :: Int]
                     , bench "[1..1000]" $ nf myReverse [1..1000 :: Int]
                     ]
  ]
