{- |
Description: Run-length encoding of a list
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P10" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P10 (encode) where

import           Data.Maybe (mapMaybe)
import           Problems.P04
import           Problems.P09

{- |
Use the 'Problems.P09.pack' function to implement
the so-called run-length encoding data compression method.

Consecutive duplicates of elements are encoded as tuples @(n, e)@,
where @n@ is the number of duplicates of the element @e@.
-}
encode :: Eq a => [a] -> [(Int, a)]
encode xs = mapMaybe count $ pack xs
  where count [] = Nothing
        count l@(x:_) = Just (myLength l, x)
