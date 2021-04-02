{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P06Bench (group) where

import           Criterion
import qualified Problems.P06  as Problem
import qualified Solutions.P06 as Solution

group :: Benchmark
group = bgroup "P06"
  [ subgroup "isPalindrome" Problem.isPalindrome
  , bgroup "Solutions"
    [ subgroup "isPalindrome" Solution.isPalindrome ]
  ]

subgroup :: String -> ([Int] -> Bool) -> Benchmark
subgroup name isPalindrome = bgroup name
  [ bgroup "True"
    [ bench "[]" $ nf isPalindrome []
    , bench "[1]" $ nf isPalindrome [1]
    , bench "[1..5,5..1]" $ nf isPalindrome $ [1..5] ++ [5,4..1]
    , bench "[1..100,101,100..1]" $ nf isPalindrome $ [1..100] ++ [101] ++ [100,99..1]
    ]
  , bgroup "False"
    [ bench "[1..10]" $ nf isPalindrome [1..10]
    , bench "[1..100,101,102,100..1]" $ nf isPalindrome $ [1..100] ++ [101, 102] ++ [100,99..1]
    ]
  ]
