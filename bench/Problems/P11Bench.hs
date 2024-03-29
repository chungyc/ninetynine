{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P11Bench (group) where

import           Criterion
import           Problems.Lists
import qualified Problems.P11   as Problem
import qualified Solutions.P11  as Solution

group :: Benchmark
group = bgroup "P11"
  [ subgroup "encodeModified" Problem.encodeModified
  , bgroup "Solutions"
    [ subgroup "encodeModified" Solution.encodeModified ]
  ]

subgroup :: String -> ([Int] -> [Encoding Int]) -> Benchmark
subgroup name encodeModified = bgroup name
  [ bench "[]"        $ nf encodeModified []
  , bench "[1]"       $ nf encodeModified [1]
  , bench "[1..10]"   $ nf encodeModified [1..10]
  , bench "[1..100]"  $ nf encodeModified [1..100]
  , bench "[1..1000]" $ nf encodeModified [1..1000]
  , bench "replicate 10 1"   $ nf encodeModified $ replicate 10 1
  , bench "replicate 100 1"  $ nf encodeModified $ replicate 100 1
  , bench "replicate 1000 1" $ nf encodeModified $ replicate 1000 1
  , bench "concatMap (replicate 10) [1..10]" $
    nf encodeModified $ concatMap (replicate 10) [1..10]
  , bench "concatMap (replicate 10) [1..100]" $
    nf encodeModified $ concatMap (replicate 10) [1..100]
  ]
