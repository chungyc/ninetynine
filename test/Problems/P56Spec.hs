{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P56Spec (spec) where

import           Problems.BinaryTrees
import           Problems.BinaryTrees.Arbitrary ()
import qualified Problems.P56                   as Problem
import qualified Solutions.P56                  as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Tree Int -> Bool) -> String -> Spec
properties symmetric name = describe name $ do
  prop "is true for trees whose structure is symmetric" $
    \t -> forAll (valued $ Branch () t $ reflect t) $ \t' ->
      t' `shouldSatisfy` symmetric

  prop "is false for non-symmetric trees" $
    \t -> \t' -> t /= reflect t' ==>
    forAll (valued $ Branch () t t') $ \t'' ->
    t'' `shouldNotSatisfy` symmetric

examples :: Spec
examples = describe "Examples" $ do
  it "symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)" $ do
    symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty) `shouldBe` False

  it "symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))" $ do
    symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)) `shouldBe` True

  where symmetric = Problem.symmetric

spec :: Spec
spec = parallel $ do
  properties Problem.symmetric "symmetric"
  examples
  describe "From solutions" $ do
    properties Solution.symmetric "symmetric"

reflect :: Tree a -> Tree a
reflect Empty                 = Empty
reflect (Branch x left right) = Branch x (reflect right) (reflect left)

-- | Generates a tree with the same structure as the given tree,
-- but with arbitrary values in each node.
valued :: Arbitrary b => Tree a -> Gen (Tree b)
valued Empty          = elements [Empty]
valued (Branch _ l r) = Branch <$> arbitrary <*> valued l <*> valued r
