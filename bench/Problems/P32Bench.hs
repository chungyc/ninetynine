module Problems.P32Bench (group) where

import           Criterion
import qualified Problems.P32  as Problem
import qualified Solutions.P32 as Solution

group :: Benchmark
group = bgroup "P32"
  [ subgroup "myGCD" Problem.myGCD
  , bgroup "Solutions"
    [ subgroup "myGCD" Solution.myGCD ]
  ]

subgroup :: String -> (Integer -> Integer -> Integer) -> Benchmark
subgroup name myGCD = bgroup name
  [ bench "  84237  66323" $ nf (myGCD   84237)  66323
  , bench "9345792  34884" $ nf (myGCD 9345792)  34884
  , bench " 129923 902039" $ nf (myGCD  129923) 902039
  ]
