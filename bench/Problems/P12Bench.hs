module Problems.P12Bench (group) where

import           Criterion
import           Problems.P11.Definitions
import           Problems.P12

group :: Benchmark
group = bgroup "P12" [
  bgroup "decodeModified" [ bench "[]" $ nf decodeModified ([] :: [Encoding Int])
                          , bench "[Single 1]" $ nf decodeModified [Single (1 :: Int)]
                          , bench "map Single [1..10]" $
                            nf decodeModified $ map Single [1..10 :: Int]
                          , bench "map Single [1..100]" $
                            nf decodeModified $ map Single [1..100 :: Int]
                          , bench "map Single [1..1000]" $
                            nf decodeModified $ map Single [1..1000 :: Int]
                          , bench "[Multiple 10 1]" $
                            nf decodeModified [Multiple 10 (1 :: Int)]
                          , bench "[Multiple 100 1]" $
                            nf decodeModified [Multiple 100 (1 :: Int)]
                          , bench "[Multiple 1000 1]" $
                            nf decodeModified [Multiple 1000 (1 :: Int)]
                          , bench "map (Multiple 10) [1..10]" $
                            nf decodeModified $ map (Multiple 10) [1..10 :: Int]
                          , bench "map (Multiple 10) [1..100]" $
                            nf decodeModified $ map (Multiple 10) [1..10 :: Int]
                          ]
  ]
