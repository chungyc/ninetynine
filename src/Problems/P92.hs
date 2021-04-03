{- |
Description: Graceful tree labeling
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P92".
-}
module Problems.P92 (gracefulTree, tree92, tree92') where

import           Data.Map.Lazy   (Map)
import           Problems.Graphs
import           Problems.P80
import qualified Solutions.P92   as Solution

{- |
It has been conjectured that if graph \(G=(V,E)\) is a tree with \(n\) vertexes,
which necessarily means that it has \(n-1\) edges, then there is
a [/graceful labeling/](https://en.wikipedia.org/wiki/Graceful_labeling) of the tree.
This means that there is a way to label each vertex with integers from 1 to \(n\)
such that for every integer \(m\) from 1 to \(n-1\), there is an edge whose difference
in vertex labels is \(m\).  I.e., there is a bijection \(l : V \rightarrow \{ k \,|\, 1 \leq k \leq n \}\)
such that \(\{ |l(v)-l(v')| \,|\, \{v,v'\} \in E \} = \{ k \,|\, 1 \leq k \leq n-1 \}\).
There is no known counterexample, but neither is it proven that this is true for all trees.

For example, the following is a graceful labeling of the tree graph 'tree92':

![With vertexes also being graceful labels, tree graph with vertexes 1,2,3,4,5,6,7 and edges (1,7), (7,3), (3,5), (5,4), (2,7), (3,6)](images/Miscellaneous/Graceful-Tree-P92.svg)

Write a function which will gracefully label a tree graph.
If there is no graceful labeling for a particular tree,
return 'Nothing', and write up the counterexample for publication.

=== Examples

>>> gracefulTree tree92
Just (fromList [(1,5),(2,2),(3,1),(4,7),(5,3),(6,6),(7,4)])

>>> gracefulTree tree92'
Just (fromList [(1,1),(2,10),(3,7),(4,3),(5,5),(6,6),(7,14),(8,13),(9,12),(10,11),(11,9),(12,2),(13,4),(14,8)])

=== __Notes__

Problem 92 in the original list referred to this conjecture as "Von Koch's conjecture".
However, I could not find any reference confirming that [von Koch](https://en.wikipedia.org/wiki/Helge_von_Koch)
actually made this conjecture.  From the style the problem was written in, it seems likely
this was an imaginary scenario to make the problem feel more personal and fun,
rather than something which actually happened.

It is too unbelievable for me to have met the mathematician in question,
so this was rewritten to a more dry presentation.
-}
gracefulTree :: G -> Maybe (Map Vertex Int)
gracefulTree = Solution.gracefulTree

-- | Tree graph used for the example in 'gracefulTree'.
tree92 :: G
tree92 = toG $ Paths [[1,2,3,4,5], [2,7], [3,6]]

-- | A 14-vertex tree graph to try 'gracefulTree' on.
--
-- ![Tree graph with vertexes 1 to 14 and edges (1,2), (2,3), (3,4), (4,5), (5,6), (1,7), (1,8), (1,9), (1,10), (11,12) (2,12), (2,13), (4,14)](images/Miscellaneous/Tree14-P92.svg)
tree92' :: G
tree92' = toG $ Paths [[1,2,3,4,5,6], [7,1,8], [9,1,10], [11,12,2,13], [4,14]]
