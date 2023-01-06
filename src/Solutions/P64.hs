{- |
Description: Binary tree layout; in-order
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P64" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P64 (layoutInorder) where

import           Problems.BinaryTrees

-- | Lay out a binary tree using in-order positions.
layoutInorder :: Tree a -> Tree (a, (Int,Int))
layoutInorder t = fst $ layout t 1 1

layout :: Tree a -> Int -> Int -> (Tree (a, (Int, Int)), Int)
layout Empty order _ = (Empty, order)
layout (Branch x l r) order depth = (Branch (x, (order', depth)) l' r', order'')
  where (l', order') = layout l order (depth+1)
        (r', order'') = layout r (order'+1) (depth+1)
