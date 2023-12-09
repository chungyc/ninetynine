{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P67Bench (group) where

import           Criterion
import           Problems.BinaryTrees
import qualified Problems.P67         as Problem
import qualified Solutions.P67        as Solution
import           System.Random

group :: Benchmark
group = bgroup "P67"
  [ treeToStringGroup "treeToString" Problem.treeToString
  , stringToTreeGroup "stringToTree" Problem.stringToTree
  , bgroup "Solutions"
    [ treeToStringGroup "treeToString" Solution.treeToString
    , stringToTreeGroup "stringToTree" Solution.stringToTree
    ]
  ]

treeToStringGroup :: String -> (Tree Char -> String) -> Benchmark
treeToStringGroup name treeToString = bgroup name
  [ bench "100"   $ nf treeToString $ generate (mkStdGen 8589345) 100
  , bench "10000" $ nf treeToString $ generate (mkStdGen 34734) 10000
  ]

stringToTreeGroup :: String -> (String -> Maybe (Tree Char)) -> Benchmark
stringToTreeGroup name stringToTree = bgroup name
  [ bench "100"   $ nf stringToTree (treeToString $ generate (mkStdGen 8589345) 100)
  , bench "10000" $ nf stringToTree (treeToString $ generate (mkStdGen 34734) 10000)
  ]
  where treeToString = Problem.treeToString

generate :: RandomGen g => g -> Int -> Tree Char
generate _ 0 = Empty
generate gen n = Branch 'x' left right
  where (m, gen') = randomR (0, n-1) gen
        (gen'', gen''') = split gen'
        left = generate gen'' m
        right = generate gen''' (n-m-1)
