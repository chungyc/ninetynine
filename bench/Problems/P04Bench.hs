module Problems.P04Bench (group) where

import           Criterion
import qualified Problems.P04  as Problem
import qualified Solutions.P04 as Solution

group :: Benchmark
group = bgroup "P04" [
  subgroup "myLength" Problem.myLength,
  bgroup "Solutions" [
      subgroup "myLength" Solution.myLength,
      subgroup "myLength'" Solution.myLength',
      subgroup "myLength''" Solution.myLength''
      ]
  ]

subgroup :: String -> ([Int] -> Int) -> Benchmark
subgroup name myLength = bgroup name [
   bench "[]" $ nf myLength [],
   bench "[1..1]" $ nf myLength [1],
   bench "[1..10]" $ nf myLength [1..10],
   bench "[1..100]" $ nf myLength [1..100],
   bench "[1..1000]" $ nf myLength [1..1000]
   ]
