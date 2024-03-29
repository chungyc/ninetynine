{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P60Spec (spec) where

import           Data.List                      (sort)
import           Problems.BinaryTrees
import           Problems.BinaryTrees.Arbitrary ()
import           Problems.P59
import qualified Problems.P60                   as Problem
import qualified Solutions.P60                  as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Int -> [Tree ()]) -> String -> Spec
properties heightBalancedTreesWithNodes name = describe name $ do
  prop "includes arbitrary height-balanced tree" $
    forAll (chooseInt (0,5)) $ \h ->
    forAll (elements $ heightBalancedTrees h) $ \t ->
    heightBalancedTreesWithNodes (treeSize t) `shouldSatisfy` elem t

  modifyMaxSize (const 8) $ do
    prop "includes tree only if it is height balanced" $ \t ->
      classify (treeHeight t < 3) "trivial" $
      classify (isHeightBalanced t) "balanced" $
      heightBalancedTreesWithNodes (treeSize t)
      `shouldSatisfy` (==) (isHeightBalanced t) . elem t

examples :: Spec
examples = describe "Examples" $ do
  it "length $ heightBalancedTreesWithNode 15" $ do
    length (heightBalancedTreesWithNodes 15) `shouldBe` 1553

  it "map heightBalancedTreesWithNode [0..3]" $ do
    -- Sort for each height to remove unnecessary order senstivity.
    map (sort . heightBalancedTreesWithNodes) [0..3] `shouldMatchList`
      map sort [ [ Empty ]
               , [ Branch () Empty Empty ]
               , [ Branch () Empty (Branch () Empty Empty)
                 , Branch () (Branch () Empty Empty) Empty
                 ]
               , [ Branch () (Branch () Empty Empty) (Branch () Empty Empty) ]
               ]

  where heightBalancedTreesWithNodes = Problem.heightBalancedTreesWithNodes

spec :: Spec
spec = parallel $ do
  properties Problem.heightBalancedTreesWithNodes "heightBalancedTreesWithNodes"
  examples
  describe "From solutions" $ do
    properties Solution.heightBalancedTreesWithNodes "heightBalancedTreesWithNodes"

isHeightBalanced :: Tree a -> Bool
isHeightBalanced Empty = True
isHeightBalanced (Branch _ l r) =
  abs (treeHeight l - treeHeight r) <= 1 &&
  isHeightBalanced l && isHeightBalanced r
