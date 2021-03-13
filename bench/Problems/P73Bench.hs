module Problems.P73Bench (group) where

import           Criterion
import           Problems.MultiwayTrees
import qualified Problems.P73           as Problem
import qualified Solutions.P73          as Solution

group :: Benchmark
group = bgroup "P73"
  [ bench "treeToSexp" $ nf Problem.treeToSexp tree
  , bench "sexpToTree" $ nf Problem.sexpToTree sexp
  , bgroup "Solutions"
    [ bench "treeToSexp" $ nf Solution.treeToSexp tree
    , bench "sexpToTree" $ nf Solution.sexpToTree sexp
    ]
  ]

tree :: MultiwayTree Char
tree = MultiwayTree 'a' $
    replicate 100 $ MultiwayTree 'b' $
    replicate 50 $ MultiwayTree 'c' [MultiwayTree 'd' []]

sexp :: String
sexp = Problem.treeToSexp tree
