module Problems.P55Spec (spec) where

import           Problems.BinaryTrees
import           Problems.BinaryTrees.Arbitrary ()
import qualified Problems.P55                   as Problem
import qualified Solutions.P55                  as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Int -> [Tree Char]) -> String -> Spec
properties completelyBalancedTrees name = do
  describe name $ do
    modifyMaxSize (const 32) $ do  -- limit combinatorial explosion
      prop "has completely balanced trees" $
        \(NonNegative n) -> mapM_ (flip shouldSatisfy balanced) (completelyBalancedTrees n)

      prop "contains only 'x'" $
        let containsExpected Empty            = True
            containsExpected (Branch 'x' t v) = containsExpected t && containsExpected v
            containsExpected _                = False
        in \(NonNegative n) -> mapM_ (flip shouldSatisfy containsExpected) (completelyBalancedTrees n)

    describe "includes all completely balanced trees" $ do
      flip mapM_ [0..8] $ do
        \n -> it ("with size " ++ show n) $ do
          (let trees 0 = [Empty]
               trees k = [Branch 'x' l r | (l, r) <- branches k]
               branches k = [(l, r) | (ls, rs) <- subtrees k, l <- ls, r <- rs]
               subtrees k = [(trees i, trees j) | i <- [0..k], j <- [0..k], i + j == (k-1)]
           in completelyBalancedTrees n `shouldMatchList` filter balanced (trees n))

  where count Empty          = (0 :: Int)
        count (Branch _ t v) = 1 + count t + count v
        balanced Empty = True
        balanced (Branch _ t v)
          | abs (count t - count v) <= 1 = balanced t && balanced v
          | otherwise                    = False

examples :: Spec
examples = do
  describe "Examples" $ do
    it "completelyBalancedTrees 4" $ do
      completelyBalancedTrees 4 `shouldMatchList`
        [ Branch 'x'
          (Branch 'x' Empty Empty)
          (Branch 'x' Empty
            (Branch 'x' Empty Empty))
        , Branch 'x'
          (Branch 'x' Empty Empty)
          (Branch 'x' (Branch 'x' Empty Empty) Empty)
        , Branch 'x'
          (Branch 'x' Empty
            (Branch 'x' Empty Empty))
          (Branch 'x' Empty Empty)
        , Branch 'x'
          (Branch 'x' (Branch 'x' Empty Empty) Empty)
          (Branch 'x' Empty Empty)
        ]

  where completelyBalancedTrees = Problem.completelyBalancedTrees

spec :: Spec
spec = parallel $ do
  properties Problem.completelyBalancedTrees "completelyBalancedTrees"
  examples
  describe "From solutions" $ do
    properties Solution.completelyBalancedTrees "completelyBalancedTrees"
