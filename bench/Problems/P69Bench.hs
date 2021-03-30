module Problems.P69Bench (group) where

import           Criterion
import           Data.List            (nub)
import           Problems.BinaryTrees
import           Problems.P57
import qualified Problems.P69         as Problem
import qualified Solutions.P69        as Solution
import           System.Random

group :: Benchmark
group = bgroup "P69"
  [ dotstringToTreeGroup "dotstringToTree" Problem.dotstringToTree
  , treeToDotstringGroup "treeToDotstring" Problem.treeToDotstring
  , bgroup "Solutions"
    [ dotstringToTreeGroup "dotstringToTree" Solution.dotstringToTree
    , treeToDotstringGroup "treeToDotstring" Solution.treeToDotstring
    ]
  ]

dotstringToTreeGroup :: String -> (String -> Tree Char) -> Benchmark
dotstringToTreeGroup name dotstringToTree = bgroup name
  [ bench "100"   $ nf dotstringToTree $ Problem.treeToDotstring $ generate (mkStdGen 8589345) 100
  , bench "10000" $ nf dotstringToTree $ Problem.treeToDotstring $ generate (mkStdGen 34734) 10000
  ]

treeToDotstringGroup :: String -> (Tree Char -> String) -> Benchmark
treeToDotstringGroup name treeToDotstring = bgroup name
  [ bench "100"  $ nf treeToDotstring $ generate (mkStdGen 8589345) 100
  , bench "1000" $ nf treeToDotstring $ generate (mkStdGen 34734) 10000
  ]

generate :: RandomGen g => g -> Int -> Tree Char
generate gen n = construct $ nub $ take n $ randoms gen
