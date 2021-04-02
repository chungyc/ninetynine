{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P64Bench (group) where

import           Criterion
import           Problems.BinaryTrees
import qualified Problems.P64         as Problem
import qualified Solutions.P64        as Solution
import           System.Random

group :: Benchmark
group = bgroup "P64"
  [ subgroup "layoutInorder" Problem.layoutInorder
  , bgroup "Solutions"
    [ subgroup "layoutInorder"  Solution.layoutInorder ]
  ]

subgroup :: String -> (Tree () -> Tree ((), (Int, Int))) -> Benchmark
subgroup name layoutInorder = bgroup name
  [ bench "100"   $ nf layoutInorder $ generate (mkStdGen 8589345) 100
  , bench "10000" $ nf layoutInorder $ generate (mkStdGen 34734) 10000
  ]

generate :: RandomGen g => g -> Int -> Tree ()
generate _ 0 = Empty
generate gen n = Branch () left right
  where (m, gen') = randomR (0, n-1) gen
        (gen'', gen''') = split gen'
        left = generate gen'' m
        right = generate gen''' (n-m-1)
