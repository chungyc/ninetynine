{- |
Description: `layoutCompact`
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P66".
-}
module Problems.P66 (layoutCompact) where

import           Problems.BinaryTrees
import qualified Solutions.P66        as Solution

-- $setup
-- >>> import Problems.P65

{- |
Yet another layout strategy is shown in the illustration below:

![Layout example](images/BinaryTrees/Layout-P66.svg)

The method yields a very compact layout while maintaining a certain symmetry in every node.
Find out the rules and write the corresponding function.
Use the same conventions as in "Problems.P64" and "Problems.P65".

=== Examples

>>> layoutCompact tree65
Branch ('n',(5,1)) (Branch ('k',(3,2)) (Branch ('c',(2,3)) ...

=== __Hint__
Consider the horizontal distance between a node and its successor nodes.
How tight can you pack together two subtrees to construct the combined binary tree?
-}
layoutCompact :: Tree a -> Tree (a, (Int, Int))
layoutCompact = Solution.layoutCompact
