module Problems.P46Bench (group) where

import           Criterion
import           Problems.Logic
import qualified Problems.P46   as Problem
import qualified Solutions.P46  as Solution

group :: Benchmark
group = bgroup "P46"
  [ subgroup "boolean functions" Problem.functions
  , bgroup "Solutions"
    [ subgroup "boolean functions" Solution.functions ]
  ]

subgroup :: String -> Functions -> Benchmark
subgroup name fs = bench name $
  nf table (\a -> \b -> xor' (equ' a (nand' (or' a b) (and' a b))) (impl' (nor' a b) b))
  where Functions { getTable = table
                  , getAnd = and'
                  , getOr = or'
                  , getNand = nand'
                  , getNor = nor'
                  , getXor = xor'
                  , getImpl = impl'
                  , getEqu = equ'
                  } = fs
