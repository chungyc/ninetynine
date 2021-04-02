{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P65Bench (group) where

import           Criterion
import           Problems.BinaryTrees
import qualified Problems.P65         as Problem
import qualified Solutions.P65        as Solution
import           System.Random

group :: Benchmark
group = bgroup "P65"
  [ subgroup "layoutLevelConstant" Problem.layoutLevelConstant
  , bgroup "Solutions"
    [ subgroup "layoutLevelConstant"  Solution.layoutLevelConstant ]
  ]

subgroup :: String -> (Tree () -> Tree ((), (Int, Int))) -> Benchmark
subgroup name layoutLevelConstant = bgroup name
  [ bench "100"   $ nf layoutLevelConstant $ generate (mkStdGen 8589345) 100
  , bench "10000" $ nf layoutLevelConstant $ generate (mkStdGen 34734) 10000
  ]

generate :: RandomGen g => g -> Int -> Tree ()
generate _ 0 = Empty
generate gen n = Branch () left right
  where (m, gen') = randomR (0, n-1) gen
        (gen'', gen''') = split gen'
        left = generate gen'' m
        right = generate gen''' (n-m-1)
