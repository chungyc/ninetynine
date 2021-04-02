{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P56Spec (spec) where

import           Problems.BinaryTrees
import           Problems.BinaryTrees.Arbitrary ()
import qualified Problems.P56                   as Problem
import qualified Solutions.P56                  as Solution
import           System.Random
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Tree Int -> Bool) -> String -> Spec
properties symmetric name = do
  describe name $ do
    prop "is true for trees whose structure is symmetric" $
      \t -> \n -> (scramble (Branch n t $ reflect t) rnd) `shouldSatisfy` symmetric

    prop "is false for non-symmetric trees" $
      \t -> \v ->
        t /= reflect v ==>
        scramble (Branch () t v) rnd `shouldNotSatisfy` symmetric

  where
    -- Flip a binary tree into its mirror image.
    reflect Empty                 = Empty
    reflect (Branch x left right) = Branch x (reflect right) (reflect left)

    -- Add arbitrary values to each node of a tree that is purely structure.
    scramble Empty _ = Empty
    scramble (Branch () left right) gen =
      let (n, gen')       = random gen
          (gen'', gen''') = split gen'
      in Branch n (scramble left gen'') (scramble right gen''')

    -- An arbitrary number generator.
    rnd = mkStdGen 137

examples :: Spec
examples = do
  describe "Examples" $ do
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
