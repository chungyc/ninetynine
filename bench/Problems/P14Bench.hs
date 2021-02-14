module Problems.P14Bench (group) where

import           Criterion
import           Problems.P14

group :: Benchmark
group = bgroup "P14" [
  bgroup "dupli" [ bench "[1]" $ nf dupli [1 :: Int]
                 , bench "[1..10]" $ nf dupli [1..10 :: Int]
                 , bench "[1..100]" $ nf dupli [1..100 :: Int]
                 , bench "[1..1000]" $ nf dupli [1..1000 :: Int]
                 ]
  ]
