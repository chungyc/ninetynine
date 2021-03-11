module Problems.P70Bench (group) where

import           Criterion
import           Problems.MultiwayTrees
import qualified Problems.P70           as Problem
import qualified Solutions.P70          as Solution

group :: Benchmark
group = bgroup "P70"
  [ stringToTreeGroup Problem.stringToTree "stringToTree"
  , treeToStringGroup Problem.treeToString "treeToString"
  , bgroup "Solutions"
    [ stringToTreeGroup Solution.stringToTree "stringToTree"
    , treeToStringGroup Solution.treeToString "treeToString"
    ]
  ]

stringToTreeGroup :: (String -> MultiwayTree Char) -> String -> Benchmark
stringToTreeGroup stringToTree name = bgroup name
  [ bench "string length 500" $ nf stringToTree (replicate 100 'a' ++
                                                 replicate 50 '^' ++
                                                 (concat $ replicate 50 "bc^d^^") ++
                                                 replicate 50 '^') ]

treeToStringGroup :: (MultiwayTree Char -> String) -> String -> Benchmark
treeToStringGroup treeToString name = bgroup name
  [ bench "tree size 301" $ nf treeToString
    (MultiwayTree 'a' $
     replicate 100 $ MultiwayTree 'b' [MultiwayTree 'c' [],
                                       MultiwayTree 'd' []]) ]
