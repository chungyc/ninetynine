module Problems.P48Spec (spec) where

import           Data.List             (nub, sort)
import           Problems.P46
import qualified Problems.P48          as Problem
import qualified Solutions.P48         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Int -> ([Bool] -> Bool) -> [[Bool]]) -> String -> Spec
properties tablen name = do
  describe name $ do
    prop "satisfies logical expression" $
      \(Fn f) -> forAll (chooseInt (0, 10)) $ \ n ->
        tablen n f `shouldSatisfy`
        all (\xs -> f (init xs) == last xs)

    prop "assigns expected number of variables" $
      \(Fn f) -> forAll (chooseInt (0, 10)) $ \n ->
        tablen n f `shouldSatisfy`
        all (\x -> length x == n + 1)

    prop "assigns all values to variables" $
      \(Fn f) -> forAll (chooseInt (0, 10)) $ \n ->
        tablen n f `shouldSatisfy`
        (==) (2^n) . length . nub . map init
        -- There are 2^n combinations of n booleans,
        -- so there should be 2^n distinct variable assignments.

examples :: Spec
examples = do
  describe "Examples" $ do
    it "tablen 3 (\\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)" $ do
      sort (tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)) `shouldMatchList`
        [ [False, False, False, False]
        , [False, False, True,  False]
        , [False, True,  False, False]
        , [False, True,  True,  True]
        , [True,  False, False, False]
        , [True,  False, True,  True]
        , [True,  True,  False, False]
        , [True,  True,  True,  True]
        ]

  where tablen = Problem.tablen

spec :: Spec
spec = parallel $ do
  properties Problem.tablen "tablen"
  examples
  describe "From solutions" $ do
    properties Solution.tablen "tablen"
