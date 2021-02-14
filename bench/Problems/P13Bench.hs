module Problems.P13Bench (group) where

import           Criterion
import           Problems.P13

group :: Benchmark
group = bgroup "P13" [
  bgroup "encodeDirect" [ bench "[]" $ nf encodeDirect ([] :: [Int])
                        , bench "[1]" $ nf encodeDirect [1 :: Int]
                        , bench "[1..10]" $ nf encodeDirect [1..10 :: Int]
                        , bench "[1..100]" $ nf encodeDirect [1..100 :: Int]
                        , bench "[1..1000]" $ nf encodeDirect [1..1000 :: Int]
                        , bench "replicate 10 1" $ nf encodeDirect $ replicate 10 (1 :: Int)
                        , bench "replicate 100 1" $ nf encodeDirect $ replicate 100 (1 :: Int)
                        , bench "replicate 1000 1" $ nf encodeDirect $ replicate 1000 (1 :: Int)
                        , bench "concat $ map (replicate 10) [1..10]" $
                          nf encodeDirect $ concat $ map (replicate 10) [1..10 :: Int]
                        , bench "concat $ map (replicate 10) [1..100]" $
                          nf encodeDirect $ concat $ map (replicate 10) [1..100 :: Int]
                        ]
  ]
