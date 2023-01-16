{- |
Description: Tree representation with s-expressions
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P73".
-}
module Problems.P73 (treeToSexp,sexpToTree) where

import           Problems.MultiwayTrees
import qualified Solutions.P73          as Solution

-- $setup
-- >>> import Problems.MultiwayTrees

-- | An [s-expression](https://en.wikipedia.org/wiki/S-expression) is
-- commonly represented as a list of items between parentheses.
-- In particular, s-expressions can be nested, e.g., @(a b (c d) e (f g))@.
-- It is used by programming languages such as [Lisp](https://lisp-lang.org/)
-- and [Scheme](https://groups.csail.mit.edu/mac/projects/scheme/).
--
-- A possible way to represent a multiway tree with an s-expression is for
-- the first element in a list to represent the root of the tree,
-- and the rest of the elements in the list would be its children.
-- As a special case, a multiway tree with no children is represented without parentheses.
--
-- Write a function which will return this s-expression representation
-- of a multiway tree as a string.
--
-- === Examples
--
-- >>> treeToSexp multitree1
-- "a"
--
-- >>> treeToSexp multitree2
-- "(a b)"
--
-- >>> treeToSexp multitree3
-- "(a (b c))"
--
-- >>> treeToSexp multitree4
-- "(b d e)"
--
-- >>> treeToSexp multitree5
-- "(a (f g) c (b d e))"
--
-- === __Notes__
--
-- This has been substantially rewritten from problem 73 in the original list,
-- although it is still really the same problem.  It made it sound like the
-- representation outlined would be a natural one in Lisp, but it is hard to find
-- multiway trees represented in this exact way with Lisp.
-- All references I have found claiming this is a common representation
-- are derived from the problem in the original list of 99 Prolog Problems.
--
-- I have tried to avoid giving an impression that this is the standard
-- representation of trees in Lisp.
treeToSexp :: MultiwayTree Char -> String
treeToSexp = Solution.treeToSexp

-- | Write a function which will convert an s-expression, representing
-- a multiway tree as in 'treeToSexp', into a 'MultiwayTree'.
--
-- === Examples
--
-- >>> sexpToTree "(a (f g) c (b d e))" == multitree5
-- True
sexpToTree :: String -> MultiwayTree Char
sexpToTree = Solution.sexpToTree
