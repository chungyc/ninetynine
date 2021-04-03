{- |
Description: Direct run-length encoding of a list
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P13" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P13 (encodeDirect) where

import           Problems.Lists

-- | Implement the so-called run-length encoding data compression method directly.
-- I.e., do not explicitly create the sublists containing the duplicates,
-- as with 'Problems.P09.pack', but only count them.
--
-- As with 'Problems.P11.encodeModified',
-- simplify the result list by replacing the singletons @('Multiple' 1 x)@ by @('Single' x)@.
encodeDirect :: Eq a => [a] -> [Encoding a]
encodeDirect [] = []
encodeDirect (x:xs) = encode (n+1) x : encodeDirect r
  where (n, r) = consume x xs

-- | Count and remove the elemement consecutively from the front of the list.
consume :: Eq a => a -> [a] -> (Int, [a])
consume _ [] = (0, [])
consume x l@(y:ys)
  | x == y = let (n, zs) = consume x ys in (n+1, zs)
  | otherwise = (0, l)

encode :: Int -> a -> Encoding a
encode 1 x = Single x
encode n x = Multiple n x
