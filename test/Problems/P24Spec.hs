module Problems.P24Spec (spec) where

import           Data.List             (isSubsequenceOf, sort, unfoldr)
import qualified Problems.P24          as Problem
import qualified Solutions.P24         as Solution
import           System.Random
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Int -> Int -> StdGen -> ([Int], StdGen)) -> String -> Spec
properties randomDraw name = do
  describe name $ do
    prop "draws given number of numbers" $
      \(Positive m) -> forAll (chooseInt (0, m)) $ \n -> \seed ->
        fst (randomDraw n m $ mkStdGen seed) `shouldSatisfy` (==) n . length

    prop "draws number from given range" $
      \(Positive m) -> forAll (chooseInt (0, m)) $ \n -> \seed ->
        fst (randomDraw n m $ mkStdGen seed) `shouldSatisfy` flip isSubsequenceOf [1..m] . sort

    modifyMaxSuccess (const 1) $ do
      prop "is random and returns new random generator" $
        -- Make a number of draws and confirm that they are random by
        -- checking at least one of them is different from another.
        -- It is theoretically possible for all of them to be the same with
        -- true random numbers, but it is vanishingly unlikely.
        --
        -- Similarly, this also tests that randomDraw returns a new random generator.
        -- If it did not, the use of the same generator would return identical draws.
        \seed -> let n = 10
                     m = 1000
                     draws = unfoldr (Just . randomDraw n m) $ mkStdGen seed
                     isRandom ls = any (\(x,y) -> x /= y) $ zip ls $ tail ls
                 in (take 10 $ draws) `shouldSatisfy` isRandom

examples :: Spec
examples = do
  describe "Examples" $ do
    it "fst $ randomDraw 6 49 $ mkStdGen 111" $ do
      (fst $ randomDraw 6 49 $ mkStdGen 111)
        `shouldSatisfy` (\l -> sort l `isSubsequenceOf` [1..49] && length l == 6)

    it "take 5 $ unfoldr (Just . randomDraw 3 100) $ mkStdGen 111" $ do
      (take 5 $ unfoldr (Just . randomDraw 3 100) $ mkStdGen 111)
        `shouldSatisfy` all (\l -> sort l `isSubsequenceOf` [1..100] && length l == 3)

    it "newStdGen >>= return . fst . randomDraw 6 49" $ do
      (newStdGen >>= return . fst . randomDraw 6 49)
        >>= (`shouldSatisfy` (\l -> sort l `isSubsequenceOf` [1..49] && length l == 6))

  where randomDraw n m g = Problem.randomDraw n m g

spec :: Spec
spec = parallel $ do
  properties Problem.randomDraw "randomDraw"
  examples
  describe "From solutions" $ do
    properties Solution.randomDraw "randomDraw"
