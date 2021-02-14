module Problems.P08Bench (group) where

import           Criterion
import           Problems.P08

group :: Benchmark
group = bgroup "P08" [
  bgroup "compress" [ bench "[]" $ nf compress ([] :: [Int])
                    , bench "[1..10]" $ nf compress [1..10 :: Int]
                    , bench "[1..100]" $ nf compress [1..100 :: Int]
                    , bench "[1..1000]" $ nf compress [1..1000 :: Int]
                    , bench "replicate 10 1" $ nf compress $ replicate 10 (1 :: Int)
                    , bench "replicate 100 1" $ nf compress $ replicate 100 (1 :: Int)
                    , bench "replicate 1000 1" $ nf compress $ replicate 1000 (1 :: Int)
                    , bench "concat $ map (replicate 10) [1..10]" $
                      nf compress $ concat $ map (replicate 10) [1..10 :: Int]
                    , bench "concat $ map (replicate 10) [1..100]" $
                      nf compress $ concat $ map (replicate 10) [1..100 :: Int]
                    ]
  ]
