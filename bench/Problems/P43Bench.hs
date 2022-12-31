{-|
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P43Bench (group) where

import           Criterion
import           Data.Complex
import qualified Problems.P43  as Problem
import qualified Solutions.P43 as Solution

group :: Benchmark
group = bgroup "P43"
  [ subgroup "gaussianDividesBy" Problem.gaussianDividesBy
  , bgroup "Solutions"
    [ subgroup "gaussianDividesBy" Solution.gaussianDividesBy ]
  ]

subgroup :: String -> (Complex Integer -> Complex Integer -> Bool) -> Benchmark
subgroup name gaussianDividesBy =
  bgroup name
  [ bench "x=(12831:+3744)*y y=(8199:+322)" $
    nf (gaussianDividesBy $ multiply (12831:+3744) (8199:+322)) (8199:+322)
  , bench "x=1+(12831:+3744)*y y=(8199:+322)" $
    nf (gaussianDividesBy $ add (1:+0) $ multiply (12831:+3744) (8199:+322)) (8199:+322)
  , bench "x=(9283489241:+12388287444)*y y=(123717473:+757577744)" $
    let y = (123717473:+757577744)
        x = multiply y (9283489241:+12388287444)
    in nf (gaussianDividesBy x) y
  , bench "x=1+(9283489241:+12388287444)*y y=(123717473:+757577744)" $
    let y = (123717473:+757577744)
        x = add (1:+0) $ multiply y (9283489241:+12388287444)
    in nf (gaussianDividesBy x) y
  ]

add :: Complex Integer -> Complex Integer -> Complex Integer
add (a :+ b) (c :+ d) = (a+c) :+ (b+d)

multiply :: Complex Integer -> Complex Integer -> Complex Integer
multiply (a :+ b) (c :+ d) = (a*c-b*d) :+ (a*d+b*c)
