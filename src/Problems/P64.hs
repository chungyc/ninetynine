{- |
Description: Binary tree layout; in-order
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P64".
-}
module Problems.P64 (layoutInorder, tree64) where

import           Problems.BinaryTrees
import qualified Solutions.P64        as Solution

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

=== Examples

>>> layoutInorder tree64
Branch ('n',(8,1)) (Branch ('k',(6,2)) (Branch ('c',(2,3)) ...

=== __Notes__

In fact, the image was rendered to SVG using the return value of 'layoutInorder'
with 'Problems.BinaryTrees.SVG.writeSVG'.  The images for "Problems.P65" and "Problems.P66"
were also rendered similarly.
-}
layoutInorder :: Tree a -> Tree (a, (Int,Int))
layoutInorder = Solution.layoutInorder

-- | Tree used as the example for "Problems.P64".
tree64 :: Tree Char
tree64 = Branch 'n'
         (Branch 'k'
           (Branch 'c'
            (Branch 'a' Empty Empty)
             (Branch 'h'
               (Branch 'g'
                 (Branch 'e' Empty Empty)
                 Empty)
               Empty))
           (Branch 'm' Empty Empty))
         (Branch 'u'
           (Branch 'p'
             Empty
             (Branch 's'
               (Branch 'q' Empty Empty)
               Empty))
           Empty)
