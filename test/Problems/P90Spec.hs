module Problems.P90Spec (spec) where

import           Data.List             (permutations, sort)
import qualified Problems.P90          as Problem
import qualified Solutions.P90         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Int -> [[Int]]) -> String -> Spec
properties queens name = do
  describe name $ do
    modifyMaxSize (const 12) $ do  -- limit combinatorial explosion
      prop "solutions have N queens in different rows" $
        \(Positive n) ->
          classify (n < 4) "trivial" $
          queens n `shouldSatisfy` all (\l -> sort l == [1..n])

      prop "does not have queens attacking each other" $
        \(Positive n) ->
          classify (n < 4) "trivial" $
          queens n `shouldSatisfy` all (peaceful n)

      prop "includes queens which do not attack each other" $
        \(Positive n) -> forAll (shuffle [1..n]) $ \s ->
          classify (n < 4 || (not $ peaceful n s)) "trivial" $
          peaceful n s `shouldBe` elem s (queens n)

    modifyMaxSize (const 8) $ do
      prop "includes all solutions" $ do
        \n -> classify (n < 4) "trivial" $
              queens n `shouldMatchList` filter (peaceful n) (sort $ permutations [1..n])

  where
    -- confirms that no queens attack each other
    peaceful n s = all (not . attacks) (pairs $ expand n s)
    expand n = zip [1..n]  -- maps from row to (column, row)
    pairs positions = [(a, b) | a <- positions, b <- positions, a /= b]

    -- whether two positions attack each other
    attacks (a, b) = sameRow a b || sameColumn a b || sameDiagonal a b

    -- whether two positions are in the same row / column / diagnoal
    sameRow (_, r1) (_, r2) = r1 == r2
    sameColumn (c1, _) (c2, _) = c1 == c2
    sameDiagonal (c1, r1) (c2, r2) = abs (c1-c2) == abs (r1 - r2)

examples :: Spec
examples = do
  describe "Examples" $ do
    it "length (queens 8)" $ do
      length (queens 8) `shouldBe` 92

    it "head $ sort $ queens 8" $ do
      head (sort $ queens 8) `shouldBe` [1,5,8,6,3,7,2,4]

    where queens = Problem.queens

spec :: Spec
spec = parallel $ do
  properties Problem.queens "queens"
  examples
  describe "From solutions" $ do
    properties Solution.queens "queens"
