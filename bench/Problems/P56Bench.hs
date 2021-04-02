{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P56Bench (group) where

import           Criterion
import           Problems.BinaryTrees
import qualified Problems.P56         as Problem
import qualified Solutions.P56        as Solution
import           System.Random

group :: Benchmark
group = bgroup "P56"
  [ subgroup "symmetric" Problem.symmetric
  , bgroup "Solutions"
    [ subgroup "symmetric" Solution.symmetric ]
  ]

subgroup :: String -> (Tree () -> Bool) -> Benchmark
subgroup name symmetric = bgroup name
  [ bench   "100" $ nf symmetric $ generate (mkStdGen 8589345) 100
  , bench "10000" $ nf symmetric $ generate (mkStdGen 34734) 10000
  ]

generate :: RandomGen g => g -> Int -> Tree ()
generate _ 0 = Empty
generate gen n = Branch () left right
  where (m, gen') = randomR (0, n-1) gen
        (gen'', gen''') = split gen'
        left = generate gen'' m
        right = generate gen''' (n-m-1)
