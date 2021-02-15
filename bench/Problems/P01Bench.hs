module Problems.P01Bench (group) where

import           Criterion
import qualified Problems.P01  as Problem
import qualified Solutions.P01 as Solution

group :: Benchmark
group = bgroup "P01" [
  subgroup "myLast" Problem.myLast,
  bgroup "Solutions" [
      subgroup "myLast" Solution.myLast
      ]
  ]

subgroup :: String -> ([Int] -> Int) -> Benchmark
subgroup name myLast = bgroup name [
   bench "[1]" $ nf myLast [1],
   bench "[1..10]" $ nf myLast [1..10],
   bench "[1..100]" $ nf myLast [1..100],
   bench "[1..1000]" $ nf myLast [1..1000]
   ]
