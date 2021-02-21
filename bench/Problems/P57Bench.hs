module Problems.P57Bench (group) where

import           Criterion
import           Problems.P54.Definitions
import qualified Problems.P57             as Problem
import qualified Solutions.P57            as Solution
import           System.Random

group :: Benchmark
group = bgroup "P57"
  [ subgroup "construct" Problem.construct
  , bgroup "Solutions"
    [ subgroup "construct" Solution.construct ]
  ]

subgroup :: String -> ([Int] -> Tree Int) -> Benchmark
subgroup name construct = bgroup name
  [ bench   "100" $ nf construct $ take   100 $ generate   100
  , bench "10000" $ nf construct $ take 10000 $ generate 10000
  ]

generate :: Int -> [Int]
generate n = take n $ map fst $ iterate extend (0, mkStdGen n)

extend :: RandomGen g => (Int, g) -> (Int, g)
extend (_, gen) = random gen
