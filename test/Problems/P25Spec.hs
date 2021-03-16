module Problems.P25Spec (spec) where

import           Data.List             (sort, unfoldr)
import qualified Problems.P25          as Problem
import qualified Solutions.P25         as Solution
import           System.Random
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: ([Int] -> StdGen -> ([Int], StdGen)) -> String -> Spec
properties randomPermute name = do
  describe name $ do
    prop "is permutation of list" $
      \xs -> \seed ->
        fst (randomPermute xs $ mkStdGen seed) `shouldSatisfy` (==) (sort xs) . sort

    modifyMaxSuccess (const 1) $ do
      prop "is random and returns new random generator" $
        -- Make a number of permutations and confirm that the permutation is random by
        -- checking at least one of them is different from another.
        -- It is theoretically possible for all of them to be the same with
        -- true random numbers, but it is vanishingly unlikely.
        --
        -- Similarly, this also tests that randomPermute returns a new random generator.
        -- If it did not, the use of the same generator would return identical selections.
        \seed -> let xs = [1..100]
                     permutations = unfoldr (Just . randomPermute xs) $ mkStdGen seed
                     isRandom ls = any (\(x,y) -> x /= y) $ zip ls $ tail ls
                 in (take 10 $ permutations) `shouldSatisfy` isRandom

examples :: Spec
examples = do
  describe "Examples" $ do
    it "fst $ randomPermute [1..10] $ mkStdGen 111" $ do
      (fst $ randomPermute [1..10 :: Int] $ mkStdGen 111)
        `shouldSatisfy` (==) [1..10] . sort

    it "take 5 $ unfoldr (Just . randomPermute ['a'..'d']) $ mkStdGen 111" $ do
      (take 5 $ unfoldr (Just . randomPermute ['a'..'d']) $ mkStdGen 111)
        `shouldSatisfy` all ((==) ['a'..'d'] . sort)

    it "newStdGen >>= return . fst . randomPermute \"abcdef\"" $ do
      (newStdGen >>= return . fst . randomPermute "abcdef")
        >>= (`shouldSatisfy` (==) "abcdef" . sort)

  where randomPermute l g = Problem.randomPermute l g

spec :: Spec
spec = parallel $ do
  properties Problem.randomPermute "randomPermute"
  examples
  describe "From solutions" $ do
    properties Solution.randomPermute "randomPermute"