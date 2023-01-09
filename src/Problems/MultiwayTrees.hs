{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

{- |
Description: Supporting definitions for multiway tree problems
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Supporting definitions for multiway tree problems.
-}
module Problems.MultiwayTrees (
  MultiwayTree (MultiwayTree),
  multitree1, multitree2, multitree3, multitree4, multitree5,
  multitreeSize,
  ) where

import           Control.DeepSeq
import           GHC.Generics    (Generic)

-- | A multiway tree is composed of a root element
-- and a (possibly empty) set of successors which are multiway trees themselves.
-- A multiway tree is never empty.  The set of successor trees is sometimes called a forest.
data MultiwayTree a = MultiwayTree a [MultiwayTree a]
  deriving (Eq, Show, Generic, NFData)

-- | Example of the following multiway tree.
--
-- ![single node 'a'](images/MultiwayTrees/tree1.svg)
--
-- >>> multitree1 == MultiwayTree 'a' []
-- True
multitree1 :: MultiwayTree Char
multitree1 = MultiwayTree 'a' []

-- | Example of the following multiway tree.
--
-- !['a' has successor 'b'](images/MultiwayTrees/tree2.svg)
--
-- >>> multitree2 == MultiwayTree 'a' [MultiwayTree 'b' []]
-- True
multitree2 :: MultiwayTree Char
multitree2 = MultiwayTree 'a' [MultiwayTree 'b' []]

-- | Example of the following multiway tree.
--
-- !['a' has successor 'b', 'b' has successor 'c'](images/MultiwayTrees/tree3.svg)
--
-- >>> multitree3 == MultiwayTree 'a' [MultiwayTree 'b' [MultiwayTree 'c' []]]
-- True
multitree3 :: MultiwayTree Char
multitree3 = MultiwayTree 'a' [MultiwayTree 'b' [MultiwayTree 'c' []]]

-- | Example of the following multiway tree.
--
-- !['b' has successors 'd', 'e'](images/MultiwayTrees/tree4.svg)
--
-- >>> multitree4 == MultiwayTree 'b' [MultiwayTree 'd' [], MultiwayTree 'e' []]
-- True
multitree4 :: MultiwayTree Char
multitree4 = MultiwayTree 'b' [MultiwayTree 'd' [], MultiwayTree 'e' []]

-- | Example of the following multiway tree.
--
-- !['a' has successors 'f', 'c', 'b', and 'f' has successor 'g', and 'b' has successors 'd', 'e'](images/MultiwayTrees/tree5.svg)
--
-- >>> :{
-- multitree5 == MultiwayTree 'a'
--   [ MultiwayTree 'f' [MultiwayTree 'g' []]
--   , MultiwayTree 'c' []
--   , MultiwayTree 'b' [MultiwayTree 'd' [], MultiwayTree 'e' []]
--   ]
-- :}
-- True
multitree5 :: MultiwayTree Char
multitree5 = MultiwayTree 'a'
  [ MultiwayTree 'f' [MultiwayTree 'g' []]
  , MultiwayTree 'c' []
  , MultiwayTree 'b' [MultiwayTree 'd' [], MultiwayTree 'e' []]
  ]

-- | Returns the number of nodes in a multiway tree.
--
-- === Examples
--
-- >>> multitreeSize multitree1
-- 1
--
-- >>> multitreeSize multitree5
-- 7
--
-- === __Notes__
--
-- This was originally problem 70C.  This is not included as a problem,
-- to avoid a numbering conflict with the somewhat unrelated problem 70 in the original list,
-- which is more interesting.
multitreeSize :: MultiwayTree a -> Int
multitreeSize (MultiwayTree _ ts) = 1 + (sum $ map multitreeSize ts)
