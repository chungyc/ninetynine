{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P91Spec (spec) where

import           Data.List             (group, sort)
import           Data.Maybe            (fromJust, isJust)
import qualified Problems.P91          as Problem
import qualified Solutions.P91         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

knightsTourProperties :: (Int -> (Int,Int) -> Maybe [(Int,Int)]) -> String -> Spec
knightsTourProperties knightsTour name = do
  describe name $ do
    -- Board sizes known to have no knight's tours.
    describe "has no tours" $ do
      it "with size 2" $ do knightsTour 2 (1,1) `shouldBe` Nothing
      it "with size 3" $ do knightsTour 3 (1,1) `shouldBe` Nothing
      it "with size 4" $ do knightsTour 4 (1,1) `shouldBe` Nothing

    -- For naive backtracking algorithms, even slightly larger sizes can take a very long time.
    modifyMaxSuccess (const 10) $
      prop "is knight's tour" $
        forAll (chooseInt(5,6)) $ \n ->
        forAll (chooseInt (1,n)) $ \x ->
        forAll (chooseInt (1,n)) $ \y ->
          let tour' = knightsTour n (x,y)
              tour = fromJust tour'
          in isJust tour' ==>
             counterexample (show tour) $
             collect n $
             conjoin $
             map (tour `shouldSatisfy`) [ all (isLegalPosition n)
                                        , \p -> all (uncurry isLegalMove) (zip p $ tail p)
                                        , isAcyclicPath
                                        , isCompletePath n
                                        , (==) (x,y) . last
                                        ]
             -- Will not check that there is no tour if Nothing,
             -- which is likely to not be possible to do in a short amount of time.

closedKnightsTourProperties :: (Int -> Maybe [(Int,Int)]) -> String -> Spec
closedKnightsTourProperties closedKnightsTour name = do
  describe name $ do
    -- Board sizes known to have no closed knight's tours.
    describe "has no tours" $ do
      it "with size 2" $ do closedKnightsTour 2 `shouldBe` Nothing
      it "with size 3" $ do closedKnightsTour 3 `shouldBe` Nothing
      it "with size 4" $ do closedKnightsTour 4 `shouldBe` Nothing
      it "with size 5" $ do closedKnightsTour 5 `shouldBe` Nothing

    -- For naive backtracking algorithms, even a 6x6 board can take minutes.
    context "with size 6" $ do
      it "has legal positions" $ do
        tour6 `shouldSatisfy` all (isLegalPosition 6)

      it "has legal moves" $ do
        tour6 `shouldSatisfy` \p -> all (uncurry isLegalMove) (zip p $ tail p)

      it "has acyclic path" $ do
        tour6 `shouldSatisfy` isAcyclicPath

      it "has all squares" $ do
        tour6 `shouldSatisfy` isCompletePath 6

  where tour6 = fromJust $ closedKnightsTour 6

examples :: Spec
examples = do
  describe "Examples" $ do
    it "knightsTour 6 (3,5)" $ do
      knightsTour 6 (3,5) `shouldSatisfy` isKnightsTour 6 . fromJust

    it "closedKnightsTour 6" $ do
      fromJust (closedKnightsTour 6) `shouldSatisfy`
        \p -> isKnightsTour 6 p &&
              head p == (1,1) &&
              last p `elem` [(2,3), (3,2)]

  where knightsTour = Problem.knightsTour
        closedKnightsTour = Problem.closedKnightsTour

spec :: Spec
spec = parallel $ do
  knightsTourProperties Problem.knightsTour "knightsTour"
  closedKnightsTourProperties Problem.closedKnightsTour "closedKnightsTour"

  examples

  describe "From solutions" $ do
    knightsTourProperties Solution.knightsTour "knightsTour"
    closedKnightsTourProperties Solution.closedKnightsTour "closedKnightsTour"

isKnightsTour :: Int -> [(Int,Int)] -> Bool
isKnightsTour n p =
  all (isLegalPosition n) p &&
  all (uncurry isLegalMove) (zip p (tail p)) &&
  isAcyclicPath p &&
  isCompletePath n p

isLegalPosition :: Int -> (Int,Int) -> Bool
isLegalPosition n (x, y) = inRange x && inRange y
  where inRange z = 1 <= z && z <= n

isLegalMove :: (Int,Int) -> (Int,Int) -> Bool
isLegalMove (x, y) (x', y') = moves == (1,2) || moves == (2,1)
  where moves = (abs (x-x'), abs (y-y'))

isAcyclicPath :: [(Int,Int)] -> Bool
isAcyclicPath p = all ((==) 1 . length) $ group $ sort p

isCompletePath :: Int -> [(Int,Int)] -> Bool
isCompletePath n p = sort p == sort [(x,y) | x <- [1..n], y <- [1..n]]
