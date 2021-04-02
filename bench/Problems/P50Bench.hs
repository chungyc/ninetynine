{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P50Bench (group) where

import           Criterion
import           Problems.P50  (countCharacters, text)
import qualified Problems.P50  as Problem
import qualified Solutions.P50 as Solution

group :: Benchmark
group = bgroup "P50"
  [ bench "huffman counts" $ nf Problem.huffman counts
  , bgroup "Solutions" [ bench "huffman counts" $ nf Solution.huffman counts ]
  ]

counts :: [(Char,Int)]
counts = countCharacters text
