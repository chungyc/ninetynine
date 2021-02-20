{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{- |
Description: 'Tree'

Definitions used by "Problems.P54".
-}
module Problems.P54.Definitions (
  Tree (Empty, Branch),
  -- * Support functions
  printTreeList,
  ) where

import           Control.DeepSeq
import           Data.List       (intercalate, sort)
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

-- | An arbitrary total ordering for 'Tree'.
--
-- Defines an order for a set of 'Tree's.  Not intended to support solving problems.
instance Ord a => Ord (Tree a) where
  compare t v = compare (encodeTree t []) (encodeTree v [])

-- | Encodes 'Tree' in a form more obviously ordered,
-- in a way which avoids distinct trees being encoded the same.
encodeTree :: Tree a -> [Bool] -> [([Bool], a)]
encodeTree Empty _ = []
encodeTree (Branch x left right) trail =
  [(trail, x)] ++ encodeTree left (False : trail) ++ encodeTree right (True : trail)

-- | Prints a list of 'Tree's.
--
-- For two lists with the same trees, except for perhaps different order,
-- the output will be the same.
--
-- Not intended to support solving problems.
printTreeList :: (Show a, Ord a) => [Tree a] -> IO ()
printTreeList ts = putStrLn $ "[ " ++ content ++ "]"
  where content = intercalate "\n, " $ map show (sort ts)
