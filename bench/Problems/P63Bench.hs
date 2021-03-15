module Problems.P63Bench (group) where

import           Criterion
import qualified Problems.P63  as Problem
import qualified Solutions.P63 as Solution

group :: Benchmark
group = bgroup "P63"
  [ bench "completeBinaryTree 30000" $ nf Problem.completeBinaryTree 30000
  , bench "isCompleteBinaryTree with tree size 30000" $ nf Problem.isCompleteBinaryTree $ tree30000
  , bgroup "Solutions"
    [ bench "completeBinaryTree 30000" $ nf Solution.completeBinaryTree 30000
    , bench "isCompleteBinaryTree with tree size 30000" $ nf Solution.isCompleteBinaryTree $ tree30000
    ]
  ]
  where tree30000 = Solution.completeBinaryTree 30000
