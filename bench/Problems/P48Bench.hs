{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P48Bench (group) where

import           Criterion
import           Problems.P46
import qualified Problems.P48  as Problem
import qualified Solutions.P48 as Solution

group :: Benchmark
group = bgroup "P48"
  [ subgroup "tablen" Problem.tablen
  , bgroup "Solutions"
    [ subgroup "tablen" Solution.tablen ]
  ]

subgroup :: String -> (Int -> ([Bool] -> Bool) -> [[Bool]]) -> Benchmark
subgroup name tablen = bench name $
  nf (tablen 10) (\[a,b,c,d,e,f,g,h,i,j] ->
                    (a `impl'` (b `and'` c) `equ'` d `nor'` (e `nand'` (f `xor'` g `impl'` h) `equ'` (i `or'` j))))
