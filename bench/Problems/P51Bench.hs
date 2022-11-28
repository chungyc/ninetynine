{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P51Bench (group) where

import           Criterion
import qualified Problems.P51  as Problem
import qualified Solutions.P51 as Solution
import           System.Random

group :: Benchmark
group = bgroup "P51"
  [ bench "corrupt gen 4 string" $ nf (Problem.corrupt gen 4) string
  , bench "errorCorrectingEncode string" $ nf Problem.errorCorrectingEncode string
  , bench "errorCorrectingDecode encoded" $ nf Problem.errorCorrectingDecode encoded
  , bgroup "Solutions"
    [ bench "corrupt gen 4 string" $ nf (Solution.corrupt gen 4) string
    , bench "errorCorrectingEncode string" $ nf Solution.errorCorrectingEncode string
    , bench "errorCorrectingDecode encoded" $ nf Solution.errorCorrectingDecode encoded
    ]
  ]

string :: [Bool]
string = [True, False, False, True, True, False, True, True, True, False, True, False]

encoded :: [Bool]
encoded = Problem.errorCorrectingEncode string

gen :: StdGen
gen = mkStdGen 12932
