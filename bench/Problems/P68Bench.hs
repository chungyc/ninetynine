{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P68Bench (group) where

import           Criterion
import           Data.List            (nub)
import           Problems.BinaryTrees
import           Problems.P57
import qualified Problems.P68         as Problem
import qualified Solutions.P68        as Solution
import           System.Random

group :: Benchmark
group = bgroup "P68"
  [ toOrderGroup "inorder" Problem.inorder
  , toOrderGroup "preorder" Problem.preorder
  , toTreeGroup "ordersToTree" Problem.ordersToTree
  , bgroup "Solutions"
    [ toOrderGroup "inorder" Solution.inorder
    , toOrderGroup "preorder" Solution.preorder
    , toTreeGroup "ordersToTree" Solution.ordersToTree
    ]
  ]

toOrderGroup :: String -> (Tree Char -> [Char]) -> Benchmark
toOrderGroup name order = bgroup name
  [ bench "100"   $ nf order $ generate (mkStdGen 8589345) 100
  , bench "10000" $ nf order $ generate (mkStdGen 34734) 10000
  ]

toTreeGroup :: String -> ([Char] -> [Char] -> Maybe (Tree Char)) -> Benchmark
toTreeGroup name ordersToTree = bgroup name
  [ bench "100"  $ nf (ordersToTree $ inorder g100)  (preorder g100)
  , bench "1000" $ nf (ordersToTree $ inorder g1000) (preorder g1000)
  ]
  where inorder = Problem.inorder
        preorder = Problem.preorder
        g100 = generate (mkStdGen 8589345) 100
        g1000 = generate (mkStdGen 34734) 1000

generate :: RandomGen g => g -> Int -> Tree Char
generate gen n = construct $ nub $ take n $ randoms gen
