{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P70Bench (group) where

import           Criterion
import           Problems.MultiwayTrees
import qualified Problems.P70           as Problem
import qualified Solutions.P70          as Solution

group :: Benchmark
group = bgroup "P70"
  [ stringToMultitreeGroup Problem.stringToMultitree "stringToMultitree"
  , multitreeToStringGroup Problem.multitreeToString "multitreeToString"
  , bgroup "Solutions"
    [ stringToMultitreeGroup Solution.stringToMultitree "stringToMultitree"
    , multitreeToStringGroup Solution.multitreeToString "multitreeToString"
    ]
  ]

stringToMultitreeGroup :: (String -> MultiwayTree Char) -> String -> Benchmark
stringToMultitreeGroup stringToMultitree name = bgroup name
  [ bench "string length 500" $ nf stringToMultitree (replicate 100 'a' ++
                                                      replicate 50 '^' ++
                                                      concat (replicate 50 "bc^d^^") ++
                                                      replicate 50 '^') ]

multitreeToStringGroup :: (MultiwayTree Char -> String) -> String -> Benchmark
multitreeToStringGroup multitreeToString name = bgroup name
  [ bench "tree size 301" $ nf multitreeToString
    (MultiwayTree 'a' $
     replicate 100 $ MultiwayTree 'b' [MultiwayTree 'c' [],
                                       MultiwayTree 'd' []]) ]
