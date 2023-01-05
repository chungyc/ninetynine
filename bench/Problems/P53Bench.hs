{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P53Bench (group) where

import           Criterion
import           Problems.Logic (Formula (..))
import qualified Problems.P53   as Problem
import qualified Solutions.P53  as Solution

group :: Benchmark
group = bgroup "P53"
  [ bench "isTheorem [!X | Y, !Y | Z] (!X | Z)" $
    nf (Problem.isTheorem [ Disjoin [ Complement $ Variable "X", Variable "Y" ]
                          , Disjoin [ Complement $ Variable "Y", Variable "Z" ]
                          ])
    (Disjoin [ Complement $ Variable "X", Variable "Z" ])
  , bench "isTheorem theory theorem" $ nf (Problem.isTheorem theory) theorem
  , bgroup "Solutions"
    [ bench "isTheorem [!X | Y, !Y | Z] (!X | Z)" $
      nf (Solution.isTheorem [ Disjoin [ Complement $ Variable "X", Variable "Y" ]
                             , Disjoin [ Complement $ Variable "Y", Variable "Z" ]
                             ])
      (Disjoin [ Complement $ Variable "X", Variable "Z" ])
    , bench "isTheorem theory theorem" $ nf (Solution.isTheorem theory) theorem
    ]
  ]

theory :: Formula
theory = [ Disjoin [x, y, z]
         , Conjoin [w, Complement v, Disjoin [Complement u, Complement y]]
         , Disjoin [Complement x, w, y]
         , Complement $ Conjoin [x, y, z]
         , Disjoin [Conjoin [Complement y, z, Complement w], Complement u]
         ]
  where x = Variable "X"
        y = Variable "Y"
        z = Variable "Z"
        u = Variable "U"
        v = Variable "V"
        w = Variable "W"

theorem :: Formula
theorem = Conjoin [ Variable "X", Variable "Y" ]
