{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P52Bench (group) where

import           Criterion
import           Problems.Logic (Formula (..))
import qualified Problems.P52   as Problem
import qualified Solutions.P52  as Solution

group :: Benchmark
group = bgroup "P52"
  [ bench "toConjunctiveNormalForm formula" $ nf Problem.toConjunctiveNormalForm formula
  , bgroup "Solutions"
    [ bench "toConjunctiveNormalForm formula" $ nf Solution.toConjunctiveNormalForm formula ]
  ]

formula :: Formula
formula = Disjoin [ Complement $ Disjoin [ x
                                     , y
                                     , Complement z
                                     , Complement $ Disjoin [u, Complement v]
                                     ]
                  , u
                  , Conjoin [ Complement x
                            , t
                            , y
                            ]
                  , Disjoin [ Complement $ Disjoin [ x, v ]
                            , Disjoin [ Complement s, Complement v ]
                            ]
                  ]
  where s = Variable "S"
        t = Variable "T"
        u = Variable "U"
        v = Variable "V"
        x = Variable "X"
        y = Variable "Y"
        z = Variable "Z"
