{- |
Description: Group into disjoint subsets
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P27" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P27 (disjointGroups) where

{- |
Given a list with the sizes of each group and list of items,
write a function to return the list of disjoint groups.
-}
disjointGroups :: [Int] -> [a] -> [[[a]]]
disjointGroups [] []     = [[]]
disjointGroups [] xs     = [[xs]]  -- implicitly another group if group sizes do not sum to length
disjointGroups (n:ns) xs = concat $ map (\(g,xs') -> map (g:) $ disjointGroups ns xs') $ extract n xs

extract :: Int -> [a] -> [([a], [a])]
extract n xs
  | length xs <= n = [(xs,[])]  -- last group may be smaller than given size
  | otherwise      = extract' n ([],xs)

extract' :: Int -> ([a], [a]) -> [([a],[a])]
extract' 0 r        = [r]
extract' _ (_, [])  = []
extract' n (g,x:xs) = (map (\(g',xs') -> (g',x:xs')) $ extract' n (g,xs)) ++ extract' (n-1) (x:g,xs)
