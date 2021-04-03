{- |
Description: Huffman codes
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P50" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P50 (huffman) where

import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

{- |
Given a list of symbols and their number of occurrences,
construct a list of the symbols and their [Huffman encoding](https://brilliant.org/wiki/huffman-encoding/).

The characters @'0'@ and @'1'@ will represent the 0 and 1 bits in the encoding.
-}
huffman :: [(Char,Int)] -> [(Char,String)]
huffman []      = []
huffman [(c,_)] = [(c,"0")]
huffman cs      = codes $ snd $ Map.findMin $ build $ initial cs

data HuffmanTree = Leaf Char | Branch HuffmanTree HuffmanTree

-- | Maps each character to its initial Huffman tree keyed by weight.
-- The map keys by (weight,character) to be able to keep multiple
-- Huffman trees that have the same weight; otherwise, it only
-- needs to be able to get the Huffman tree with minimum weight.
--
-- A proper priority queue would usually be better to use here.
-- It is not done here to avoid having to implement a priority queue
-- or depend on another package.
initial :: [(Char,Int)] -> Map (Int,Char) HuffmanTree
initial cs = Map.fromList $ map (\(c,n) -> ((n,c), Leaf c)) cs

-- | Extract two Huffman trees with minimum weight and combine them into a single tree,
-- and repeat until there is only one tree left.
build :: Map (Int,Char) HuffmanTree -> Map (Int,Char) HuffmanTree
build m | Map.size m < 2 = m
        | otherwise      = build $ Map.insert (w+w',c) (Branch t t') m''
  where (((w,c), t), m') = Map.deleteFindMin m
        (((w',_), t'), m'') = Map.deleteFindMin m'

-- | Turn a Huffman tree into the concrete encoding for each character.
codes :: HuffmanTree -> [(Char,String)]
codes (Leaf c) = [(c,"")]
codes (Branch l r) = prepend '0' (codes l) ++ prepend '1' (codes r)
  where prepend b es = map (\(c,e) -> (c, b:e)) es
