module Problems.P11Bench (group) where

import           Criterion
import           Problems.P11

group :: Benchmark
group = bgroup "P11" [
  bgroup "encodeModified" [ bench "[]" $ whnf encodeModified ([] :: [Int])
                          , bench "[1]" $ nf encodeModified [1 :: Int]
                          , bench "[1..10]" $ nf encodeModified [1..10 :: Int]
                          , bench "[1..100]" $ nf encodeModified [1..100 :: Int]
                          , bench "[1..1000]" $ nf encodeModified [1..1000 :: Int]
                          , bench "replicate 10 1" $ nf encodeModified $ replicate 10 (1 :: Int)
                          , bench "replicate 100 1" $ nf encodeModified $ replicate 100 (1 :: Int)
                          , bench "replicate 1000 1" $ nf encodeModified $ replicate 1000 (1 :: Int)
                          , bench "concat $ map (replicate 10) [1..10]" $
                            nf encodeModified $ concat $ map (replicate 10) [1..10 :: Int]
                          , bench "concat $ map (replicate 10) [1..100]" $
                            nf encodeModified $ concat $ map (replicate 10) [1..100 :: Int]
                          ]
  ]
