{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{- |
Description: 'Tree'

Definitions used by "Problems.P54".
-}
module Problems.P54.Definitions (Tree (Empty, Branch)) where

import           Control.DeepSeq
import           GHC.Generics    (Generic)

-- | Binary tree.
--
-- A 'Tree' of type @a@ consists of either an 'Empty' node,
-- or a 'Branch' containing one value of type @a@ with exactly two subtrees of type @a@.
--
-- &#129335;
--
-- === __Notes__
--
-- This is not the problem 54A from the original Ninety-Nine Haskell Problems.
-- As it also mentions, there is nothing to do here except making sure code
-- compiles correctly, thanks to Haskell's strict typing and the way 'Tree'
-- is defined.
--
-- Instead, the problem was replaced by the simple problems of implementing
-- given binary trees as Haskell values.  I.e., turn the examples from
-- the original problem into simple problems to solve.
data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Eq, Show, Generic, NFData)
