{- |
Description: 'layoutInorder'

Some solutions to "Problems.P64" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P64 (layoutInorder) where

import           Problems.BinaryTrees

{- |
Let's say we want to draw a binary tree.  In preparation, a layout algorithm is
required to determine the position of each node in a rectangular grid.
Several layout methods are conceivable.  One of them is shown in the illustration below:

![Layout example](images/BinaryTrees/Layout-P64.svg)

In this layout strategy, the position of a node \(v\) is obtained by the following two rules:

* \(x(v)\) is equal to the position of the node \(v\) in the in-order sequence.
* \(y(v)\) is equal to the depth of the node \(v\) in the tree.

Write a function to annotate each node of the tree with a position,
where \((1,1)\) is the top left corner of the rectangle bounding the drawn tree.
-}
layoutInorder :: Tree a -> Tree (a, (Int,Int))
layoutInorder t = fst $ layout t 1 1

layout :: Tree a -> Int -> Int -> (Tree (a, (Int, Int)), Int)
layout Empty order _ = (Empty, order)
layout (Branch x l r) order depth = (Branch (x, (order', depth)) l' r', order'')
  where (l', order') = layout l order (depth+1)
        (r', order'') = layout r (order'+1) (depth+1)
