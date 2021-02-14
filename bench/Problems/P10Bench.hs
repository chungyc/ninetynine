module Problems.P10Bench (group) where

import           Criterion
import           Problems.P10

group :: Benchmark
group = bgroup "P10" [
  bgroup "encode" [ bench "[]" $ nf encode ([] :: [Int])
                  , bench "[1]" $ nf encode [1 :: Int]
                  , bench "[1..10]" $ nf encode [1..10 :: Int]
                  , bench "[1..100]" $ nf encode [1..100 :: Int]
                  , bench "[1..1000]" $ nf encode [1..1000 :: Int]
                  , bench "replicate 10 1" $ nf encode $ replicate 10 (1 :: Int)
                  , bench "replicate 100 1" $ nf encode $ replicate 100 (1 :: Int)
                  , bench "replicate 1000 1" $ nf encode $ replicate 1000 (1 :: Int)
                  , bench "concat $ map (replicate 10) [1..10]" $
                    nf encode $ concat $ map (replicate 10) [1..10 :: Int]
                  , bench "concat $ map (replicate 10) [1..100]" $
                    nf encode $ concat $ map (replicate 10) [1..100 :: Int]
                  ]
  ]
