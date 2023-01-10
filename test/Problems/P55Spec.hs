{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P55Spec (spec) where

import           Control.Monad                  (forM_)
import           Problems.BinaryTrees
import           Problems.BinaryTrees.Arbitrary ()
import qualified Problems.P55                   as Problem
import qualified Solutions.P55                  as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Int -> [Tree ()]) -> String -> Spec
properties completelyBalancedTrees name = describe name $ do
  modifyMaxSize (const 32) $  -- limit combinatorial explosion
    prop "has completely balanced trees" $
    \(NonNegative n) ->
      conjoin $ map (\t -> t `shouldSatisfy` balanced) $
      completelyBalancedTrees n

  describe "includes all completely balanced trees" $ do
    forM_ [0..8] $ \n -> it ("with size " ++ show n) $ do
      completelyBalancedTrees n
        `shouldMatchList` filter balanced (trees n)

  where
    -- Number of nodes in a tree.
    count Empty          = 0 :: Int
    count (Branch _ t v) = 1 + count t + count v

    -- Whether a tree is completely balanced.
    balanced Empty = True
    balanced (Branch _ t v)
      | abs (count t - count v) <= 1 = balanced t && balanced v
      | otherwise                    = False

    -- List of trees with a given number of nodes.
    trees 0 = [Empty]
    trees k = do
      i <- [0..k-1]
      let j = k - i - 1
      l <- trees i
      r <- trees j
      return $ Branch () l r

examples :: Spec
examples = do
  describe "Examples" $ do
    it "completelyBalancedTrees 4" $ do
      completelyBalancedTrees 4 `shouldMatchList`
        [ Branch ()
          (Branch () Empty Empty)
          (Branch () Empty
            (Branch () Empty Empty))
        , Branch ()
          (Branch () Empty Empty)
          (Branch () (Branch () Empty Empty) Empty)
        , Branch ()
          (Branch () Empty
            (Branch () Empty Empty))
          (Branch () Empty Empty)
        , Branch ()
          (Branch () (Branch () Empty Empty) Empty)
          (Branch () Empty Empty)
        ]

  where completelyBalancedTrees = Problem.completelyBalancedTrees

spec :: Spec
spec = parallel $ do
  properties Problem.completelyBalancedTrees "completelyBalancedTrees"
  examples
  describe "From solutions" $ do
    properties Solution.completelyBalancedTrees "completelyBalancedTrees"
