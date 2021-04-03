{- |
Description: Pack duplicates in a list
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P09" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P09 (pack, pack') where

import           Data.List (group)

-- | Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements, they should be placed in separate sublists.
--
-- Extract consecutive duplicates from the list, and repeat.
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs = duplicates : pack remainder
  where (duplicates, remainder) = extract xs

-- | Extract consecutive duplicates in front of the list into their own list.
-- Returns the consecutive duplicates in the first element,
-- and the remainder of the list in the second element.
extract :: Eq a => [a] -> ([a], [a])
extract (x : ys@(y : _))
  | x == y    = let (d, r) = extract ys in (x : d, r)
  | otherwise = ([x], ys)
extract l = (l, [])

-- | Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements, they should be placed in separate sublists.
--
-- Cheat by using 'group'.
pack' :: Eq a => [a] -> [[a]]
pack' = group
