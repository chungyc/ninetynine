{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P07Bench (group) where

import           Criterion
import           Problems.Lists
import qualified Problems.P07   as Problem
import qualified Solutions.P07  as Solution
import           System.Random

group :: Benchmark
group = bgroup "P07"
  [ subgroup "flatten" Problem.flatten
  , bgroup "Solutions"
    [ subgroup "flatten"  Solution.flatten
    , subgroup "flatten'" Solution.flatten']
  ]

subgroup :: String -> (NestedList Int -> [Int]) -> Benchmark
subgroup name flatten = bgroup name
  [ bgroup "Flat"
    [ bench "0"    $ nf flatten $ get 0    flatLists
    , bench "1"    $ nf flatten $ get 1    flatLists
    , bench "10"   $ nf flatten $ get 10   flatLists
    , bench "100"  $ nf flatten $ get 100  flatLists
    , bench "1000" $ nf flatten $ get 1000 flatLists
    ]
  , bgroup "Nested"
    [ bench "1"      $ nf flatten $ get 1    nestedLists
    , bench "10"     $ nf flatten $ get 10   nestedLists
    , bench "100"    $ nf flatten $ get 100  nestedLists
    , bench "1000"   $ nf flatten $ get 1000 nestedLists
    ]
  , bgroup "Random"
    [ bench "10"   $ nf flatten $ fst $ generate 10   $ mkStdGen 4324
    , bench "100"  $ nf flatten $ fst $ generate 100  $ mkStdGen 9251
    , bench "1000" $ nf flatten $ fst $ generate 1000 $ mkStdGen 4430
    ]
  ]

-- | Gets n'th element in list.  Indexing is 0-based.
get :: Int -> [NestedList Int] -> NestedList Int
get n xs = head $ drop n xs

-- [List [], List [Elem 1], List [Elem 1, Elem 1], List [Elem 1, Elem 1, Elem 1], ...]
flatLists :: [NestedList Int]
flatLists = map List $ iterate (Elem 1 :) []

-- [Elem 1, List [Elem 1], List [List [Elem 1]], List [List [List [Elem 1]]], ...]
nestedLists :: [NestedList Int]
nestedLists = iterate (\x -> List [x]) $ Elem 1

-- | Generates a random 'NestedList' with n elements.
generate :: RandomGen g => Int -> g -> (NestedList Int, g)
generate 0 g = (List [], g)
generate 1 g = singleton nest g'
  where (nest, g') = random g
generate n g = (combine nest (listify l1) (listify l2), g'''')
  where (m, g') = randomR (0, n) g
        (nest, g'') = random g'
        (l1, g''') = generate m g''
        (l2, g'''') = generate (n-m) g'''

singleton :: RandomGen g => Bool -> g -> (NestedList Int, g)
singleton False g = (Elem x, g')
  where (x, g') = randomR (-1000, 1000) g
singleton True g = (List [xs], g')
  where (xs, g') = generate 1 g

listify :: NestedList Int -> [NestedList Int]
listify x@(Elem _) = [x]
listify (List x)   = x

combine :: Bool -> [NestedList Int] -> [NestedList Int] -> NestedList Int
combine False xs ys = List $ xs ++ ys
combine True xs ys  = List $ (List xs) : ys
