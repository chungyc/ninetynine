{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{- |
Description: Binary tree layout; compact
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P66" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P66 (layoutCompact) where

import           Problems.BinaryTrees

{- |
Yet another layout strategy is shown in the illustration below:

![Layout example](images/BinaryTrees/Layout-P66.svg)

The method yields a very compact layout while maintaining a certain symmetry in every node.
Find out the rules and write the corresponding function.
Use the same conventions as in "Problems.P64" and "Problems.P65".
-}
layoutCompact :: Tree a -> Tree (a, (Int, Int))
layoutCompact t = place t' (1 - leftMost t') 1
  where t' = layout t

-- | Horizontal position of node, relative to parent.
type Position = Int

-- | Leftmost and rightmost positions in subtree, relative to parent.
type Bounds = [(Int,Int)]

layout :: Tree a -> Tree (a, Position, Bounds)
layout Empty          = Empty
layout (Branch x Empty Empty) = Branch (x, 0, []) Empty Empty

-- Place left subtree one unit to the left.
layout (Branch x l Empty) = Branch val l' Empty
  where val = (x, 0, (-1,-1) : shiftBounds ps' (-1))
        l' = Branch (x', -1, ps') ll' lr'
        (Branch (x', _, ps') ll' lr') = layout l

-- Place right subtree one unit to the left.
layout (Branch x Empty r) = Branch val Empty r'
  where val = (x, 0, (1,1) : shiftBounds ps' 1)
        r' = Branch (x', 1, ps') rl' rr'
        (Branch (x', _, ps') rl' rr') = layout r

layout (Branch x l r) = Branch (x, 0, ps) l' r'
  where
    -- Find the leftmost and rightmost bounds for each subtree.
    lps' = shiftBounds lps (-d)
    rps' = shiftBounds rps d
    -- Move them far enough apart that the rightmost nodes in the left subtree
    -- do not overlap with the leftmost nodes in the right subtree.
    d = safeDistance lps rps
    -- Get leftmost and rightmost bounds for this tree from those of the subtrees.
    ps = (-d,d) : mergeBounds lps' rps'
    l' = Branch (lx, -d, lps) ll lr
    r' = Branch (rx, d, rps) rl rr
    (Branch (lx, _, lps) ll lr) = layout l
    (Branch (rx, _, rps) rl rr) = layout r

-- Turn relative positions between parent and child nodes into absolute positions.
place :: Tree (a, Position, Bounds) -> Int -> Int -> Tree (a, (Int, Int))
place Empty _ _ = Empty
place (Branch (x, p, _) l r) pos depth = Branch (x, (p', depth)) l' r'
  where p' = pos + p
        l' = place l p' (depth+1)
        r' = place r p' (depth+1)

-- Find the leftmost position in the tree.
leftMost :: Tree (a, Position, Bounds) -> Int
leftMost Empty                   = 0
leftMost (Branch (_, p, ps) _ _) = minimum $ p : map fst ps

shiftBounds :: Bounds -> Int -> Bounds
shiftBounds ps d = map (\(l,r) -> (l+d,r+d)) ps

mergeBounds :: Bounds -> Bounds -> Bounds
mergeBounds [] []                      = []
mergeBounds [] ps                      = ps
mergeBounds ps []                      = ps
mergeBounds ((l,_) : ps) ((_,r) : ps') = (l,r) : mergeBounds ps ps'

safeDistance :: [(Int,Int)] -> [(Int,Int)] -> Int
safeDistance ps ps' = find 1
  where find d | isSafe ps ps' d = d
               | otherwise       = find (d+1)

isSafe :: [(Int,Int)]  -- Leftmost and rightmost positions for first subtree.
       -> [(Int,Int)]  -- Leftmost and rightmost positions for second subtree.
       -> Int          -- Horizontal distance between parent and child to try.
       -> Bool         -- If distance is enough to prevent overlaps.
isSafe [] _ _                          = True
isSafe _ [] _                          = True
isSafe ((_,r) : ps) ((l,_) : ps') dist = (r - dist) < (l + dist) && isSafe ps ps' dist
