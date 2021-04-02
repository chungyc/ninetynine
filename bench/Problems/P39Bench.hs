{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P39Bench (group) where

import           Criterion
import qualified Problems.P39  as Problem
import qualified Solutions.P39 as Solution

group :: Benchmark
group = bgroup "P39"
  [ bench "primesR 20000 21000" $ nf (Problem.primesR 20000) (21000 :: Integer)
  , bench "take 20000 $ primes" $ nf (take 20000) (Problem.primes :: [Integer])
  , bgroup "From solutions"
    [ bench "primesR 20000 21000" $ nf (Solution.primesR 20000) (21000 :: Integer)
    , bench "take 20000 $ primes" $ nf (take 20000) (Solution.primes :: [Integer])
    ]
  ]
