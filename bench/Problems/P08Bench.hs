module Problems.P08Bench (group) where

import           Criterion
import           Problems.P08

group :: Benchmark
group = bgroup "P08" [
  bgroup "compress" [ bench "[]" $ nf compress ([] :: [Int])
                    , bench "[1..10]" $ nf compress [1..10 :: Int]
                    , bench "[1..100]" $ nf compress [1..100 :: Int]
                    , bench "[1..1000]" $ nf compress [1..1000 :: Int]
                    , bench "take 10 [1,1..]" $ nf compress $ take 10 ([1,1..] :: [Int])
                    , bench "take 100 [1,1..]" $ nf compress $ take 100 ([1,1..] :: [Int])
                    , bench "take 1000 [1,1..]" $ nf compress $ take 1000 ([1,1..] :: [Int])
                    , bench "concat $ map (replicate 10) [1..10]" $
                      nf compress $ concat $ map (replicate 10) [1..10 :: Int]
                    , bench "concat $ map (replicate 10) [1..100]" $
                      nf compress $ concat $ map (replicate 10) [1..100 :: Int]
                    ]
  ]
