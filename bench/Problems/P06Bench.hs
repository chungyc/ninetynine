module Problems.P06Bench (group) where

import           Criterion
import           Problems.P06

group :: Benchmark
group = bgroup "P06" [
  bgroup "isPalindrome" [
      bgroup "True" [ bench "0"   $ nf isPalindrome ([] :: [Int])
                    , bench "1"   $ nf isPalindrome [1 :: Int]
                    , bench "10"  $ nf isPalindrome $ [1..5] ++ [5,4..1 :: Int]
                    , bench "201" $ nf isPalindrome $ [1..100] ++ [101 :: Int] ++ [100,99..1]
                    ],
      bgroup "False" [ bench "10"  $ nf isPalindrome [1..10 :: Int]
                     , bench "200" $ nf isPalindrome $ [1..100] ++ [101, 102] ++ [100,99..1 :: Int]
                     ]
      ]
  ]
