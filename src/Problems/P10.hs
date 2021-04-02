{- |
Description: 'encode'
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P10".
-}
module Problems.P10 (encode) where

import qualified Solutions.P10 as Solution

-- | Run-length encoding of a list.
--
-- Consecutive duplicates of elements are encoded as tuples @(n, e)@,
-- where @n@ is the number of duplicates of the element @e@.
--
-- Use the 'Problems.P09.pack' function to implement
-- the so-called run-length encoding data compression method.
--
-- === Examples
--
-- >>> encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
encode :: Eq a => [a] -> [(Int, a)]
encode = Solution.encode
