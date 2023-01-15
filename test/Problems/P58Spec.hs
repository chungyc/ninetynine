{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P58Spec (spec) where

import           Problems.BinaryTrees
import           Problems.BinaryTrees.Arbitrary ()
import           Problems.P56
import qualified Problems.P58                   as Problem
import qualified Solutions.P58                  as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Int -> [Tree ()]) -> String -> Spec
properties symmetricBalancedTrees name = describe name $ do
  modifyMaxSize (const 32) $ do
    prop "are all completely balanced" $
      \(NonNegative n) -> symmetricBalancedTrees n `shouldSatisfy` all balanced

    prop "are all symmetric" $
      \(NonNegative n) -> symmetricBalancedTrees n `shouldSatisfy` all symmetric

    prop "contains completely balanced trees which are symmetric" $ \t ->
      classify (balanced t) "balanced" $
      classify (symmetric t) "symmetric" $
      classify (balanced t && symmetric t) "balanced and symmetric" $
      symmetricBalancedTrees (treeSize t)
      `shouldSatisfy` (==) (balanced t && symmetric t) . elem t

  where balanced Empty = True
        balanced (Branch _ t v)
          | abs (treeSize t - treeSize v) <= 1 = balanced t && balanced v
          | otherwise = False

examples :: Spec
examples = describe "Examples" $ do
  it "symmetricBalancedTrees 5" $ do
    symmetricBalancedTrees 5 `shouldMatchList`
      [ Branch () (Branch () Empty (Branch () Empty Empty))
                  (Branch () (Branch () Empty Empty) Empty)
      , Branch () (Branch () (Branch () Empty Empty) Empty)
                  (Branch () Empty (Branch () Empty Empty))
      ]

  where symmetricBalancedTrees = Problem.symmetricBalancedTrees

spec :: Spec
spec = parallel $ do
  properties Problem.symmetricBalancedTrees "symmetricBalancedTrees"
  examples
  describe "From solutions" $ do
    properties Solution.symmetricBalancedTrees "symmetricBalancedTrees"
