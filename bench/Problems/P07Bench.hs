module Problems.P07Bench (group) where

import           Criterion
import           Problems.P07
import           Problems.P07.Definitions
import           System.Random

group :: Benchmark
group = bgroup "P07" [
  bgroup "flatten" [
      bgroup "Flat" [ bench "0"    $ nf flatten $ get 0 flatLists
                    , bench "1"    $ nf flatten $ get 1 flatLists
                    , bench "10"   $ nf flatten $ get 10 flatLists
                    , bench "100"  $ nf flatten $ get 100 flatLists
                    , bench "1000" $ nf flatten $ get 1000 flatLists
                    ],
      bgroup "Nested" [ bench "1"      $ nf flatten $ get 1 nestedLists
                      , bench "10"     $ nf flatten $ get 10 nestedLists
                      , bench "100"    $ nf flatten $ get 100 nestedLists
                      , bench "1000"   $ nf flatten $ get 1000 nestedLists
                      ]
      ],
      bgroup "Random" [ bench "10"   $ nf flatten $ fst $ generate 10 $ mkStdGen 4324
                      , bench "100"  $ nf flatten $ fst $ generate 100 $ mkStdGen 925
                      , bench "1000" $ nf flatten $ fst $ generate 1000 $ mkStdGen 443
                      ]
  ]

get :: Int -> [NestedList Int] -> NestedList Int
get n xs = head $ drop n xs

flatLists :: [NestedList Int]
flatLists = map List $ iterate (Elem 1 :) []

nestedLists :: [NestedList Int]
nestedLists = iterate (\x -> List [x]) $ Elem 1

generate :: RandomGen g => Int -> g -> (NestedList Int, g)
generate 0 g = (List [], g)
generate 1 g = let (x, g') = randomR (-1000 :: Int, 1000 :: Int) g
                   (nest, g'') = random g'
               in if nest
                  then let (xs, g''') = generate 1 g''
                       in (List [xs], g''')
                  else (Elem x, g'')
generate n g = let (m, g') = randomR (0, n) g
                   (nest, g'') = random g'
                   (begin, g''') = generate m g''
                   (end, g'''') = generate (n-m) g'''
               in (combine nest begin end, g'''')

combine :: Bool -> NestedList Int -> NestedList Int -> NestedList Int
combine _ x@(Elem _) y@(Elem _)   = List [x, y]
combine _ x@(Elem _) (List xs)    = List $ x : xs
combine _ (List xs) x@(Elem _)    = List $ xs ++ [x]
combine False (List []) xs        = xs
combine False (List xs) (List ys) = List $ xs ++ ys
combine True xs (List ys)         = List $ (List [xs]) : ys
