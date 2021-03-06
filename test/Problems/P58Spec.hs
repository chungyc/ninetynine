module Problems.P58Spec (spec) where

import           Problems.BinaryTrees
import           Problems.BinaryTrees.Arbitrary ()
import           Problems.P56
import qualified Problems.P58                   as Problem
import qualified Solutions.P58                  as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Int -> [Tree Char]) -> String -> Spec
properties symmetricBalancedTrees name = modifyMaxSize (const 32) $ do
  describe name $ do
    prop "are all completely balanced" $
      \(Positive n) -> symmetricBalancedTrees n `shouldSatisfy` all balanced

    prop "are all symmetric" $
      \(Positive n) -> symmetricBalancedTrees n `shouldSatisfy` all symmetric

    prop "contains completely balanced trees which are symmetric" $
      \t -> classify (balanced t) "balanced" $
            classify (symmetric t) "symmetric" $
            classify (balanced t && symmetric t) "balanced and symmetric" $
            xify (t :: Tree ()) `elem` symmetricBalancedTrees (count t) `shouldBe` balanced t && symmetric t

  where count Empty          = (0 :: Int)
        count (Branch _ t v) = 1 + count t + count v
        balanced Empty = True
        balanced (Branch _ t v)
          | abs (count t - count v) <= 1 = balanced t && balanced v
          | otherwise                    = False
        xify Empty          = Empty
        xify (Branch _ t v) = Branch 'x' (xify t) (xify v)

examples :: Spec
examples = do
  describe "Examples" $ do
    it "symmetricBalancedTrees 5" $ do
      symmetricBalancedTrees 5 `shouldMatchList`
        [ Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) Empty)
        , Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))
        ]

  where symmetricBalancedTrees = Problem.symmetricBalancedTrees

spec :: Spec
spec = do
  properties Problem.symmetricBalancedTrees "symmetricBalancedTrees"
  examples
  describe "From solutions" $ do
    properties Solution.symmetricBalancedTrees "symmetricBalancedTrees"
