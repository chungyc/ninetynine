module Problems.P09Bench (group) where

import           Criterion
import           Problems.P09

group :: Benchmark
group = bgroup "P09" [
  bgroup "pack" [ bench "[]" $ nf pack ([] :: [Int])
                , bench "[1]" $ nf pack [1 :: Int]
                , bench "[1..10]" $ nf pack [1..10 :: Int]
                , bench "[1..100]" $ nf pack [1..100 :: Int]
                , bench "[1..1000]" $ nf pack [1..1000 :: Int]
                , bench "replicate 10 1" $ nf pack $ replicate 10 (1 :: Int)
                , bench "replicate 100 1" $ nf pack $ replicate 100 (1 :: Int)
                , bench "replicate 1000 1" $ nf pack $ replicate 1000 (1 :: Int)
                , bench "concat $ map (replicate 10) [1..10]" $
                  nf pack $ concat $ map (replicate 10) [1..10 :: Int]
                , bench "concat $ map (replicate 10) [1..100]" $
                  nf pack $ concat $ map (replicate 10) [1..100 :: Int]
                ]
  ]
