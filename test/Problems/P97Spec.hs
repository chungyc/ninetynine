{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P97Spec (spec) where

import           Data.Ix               (inRange)
import           Data.List             (sort, transpose)
import           Data.Maybe            (fromJust)
import qualified Problems.P97          as Problem
import qualified Solutions.P97         as Solution
import           System.Random
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Gen   (Gen (MkGen))

properties :: ([[Int]] -> Maybe [[Int]]) -> String -> Spec
properties sudoku name = describe name $ do
  modifyMaxSize (const 81) $ do
    -- To generate an arbitrary puzzle with no solution, we need a working solver.
    -- If we use the solver to verify that a puzzle has no solution,
    -- the property would be a tautology, even if the solver is wrong.
    -- So we simply test that a puzzle known to have no solution returns Nothing.
    it "does not find impossible solution" $ do
      sudoku (replicate 9 [1..9]) `shouldBe` Nothing

    prop "has valid numbers" $ \(SudokuPuzzle p) ->
      sudoku p `shouldSatisfy` all (all $ inRange (1,9)) . fromJust

    prop "has valid table size" $ \(SudokuPuzzle p) ->
      conjoin [ sudoku p `shouldSatisfy` (==) 9 . length . fromJust
              , sudoku p `shouldSatisfy` all ((==) 9 . length) . fromJust
              ]

    prop "fills in blank spots" $ \(SudokuPuzzle p) ->
      let s = fromJust $ sudoku p
          lpairs = zip p s
          npairs = concatMap (uncurry zip) lpairs
      in counterexample (show s) $
         conjoin $ do
           np <- npairs
           return $ np `shouldSatisfy` \(n, n') -> n == 0 || n == n'

    prop "has each number appearing once in each row" $ \(SudokuPuzzle p) ->
      sudoku p `shouldSatisfy` isValidGroup . groupByRow . fromJust

    prop "has each number appearing once in each column" $ \(SudokuPuzzle p) ->
      sudoku p `shouldSatisfy` isValidGroup . groupByColumn . fromJust

    prop "has each number appearing once in each square" $ \(SudokuPuzzle p) ->
      sudoku p `shouldSatisfy` isValidGroup . groupBySquare . fromJust

examples :: Spec
examples = do
  describe "Examples" $ do
    it "sudoku sudokuPuzzle" $ do
      sudoku sudokuPuzzle `shouldBe` Just
        [ [ 9, 3, 4, 8, 2, 5, 6, 1, 7 ]
        , [ 6, 7, 2, 9, 1, 4, 8, 5, 3 ]
        , [ 5, 1, 8, 6, 3, 7, 9, 2, 4 ]
        , [ 3, 2, 5, 7, 4, 8, 1, 6, 9 ]
        , [ 4, 6, 9, 1, 5, 3, 7, 8, 2 ]
        , [ 7, 8, 1, 2, 6, 9, 4, 3, 5 ]
        , [ 1, 9, 7, 5, 8, 2, 3, 4, 6 ]
        , [ 8, 5, 3, 4, 7, 6, 2, 9, 1 ]
        , [ 2, 4, 6, 3, 9, 1, 5, 7, 8 ]
        ]
        -- sudokupuzzle has unique solution

  where sudoku = Problem.sudoku
        sudokuPuzzle = Problem.sudokuPuzzle

spec :: Spec
spec = parallel $ do
  properties Problem.sudoku "sudoku"
  examples
  describe "From solutions" $ do
    properties Solution.sudoku "sudoku"

groupByRow :: [[Int]] -> [[Int]]
groupByRow = id

groupByColumn :: [[Int]] -> [[Int]]
groupByColumn = transpose

groupBySquare :: [[Int]] -> [[Int]]
groupBySquare [] = []
groupBySquare (l:l':l'':ls) = fromSquareRow l l' l'' ++ groupBySquare ls
  where fromSquareRow [] [] [] = []
        fromSquareRow (x:x':x'':xs) (y:y':y'':ys) (z:z':z'':zs) =
          [x,x',x'',y,y',y'',z,z',z''] : fromSquareRow xs ys zs
        fromSquareRow _ _ _ = undefined
groupBySquare _ = undefined  -- should be impossible with test case setup

-- | Verifies whether each number appears only once in each group.
isValidGroup :: [[Int]] -> Bool
isValidGroup ls = map sort ls == replicate 9 [1..9]

-- | Arbitrary Sudoku puzzles.  Will always have at least one solution.
newtype SudokuPuzzle = SudokuPuzzle [[Int]] deriving Show

-- Ordinarily one would not use the code being tested to generate test cases.
-- However, very simple methods to generate Sudoku test cases are too slow.
-- Sufficiently efficient methods are equivalent to using a Sudoku solver
-- which can randomly choose a solution if there are multiple possible solutions,
-- and are complex enough that they would need testing of their own.
-- This would again bring up the question of how we would generate test cases for these.
--
-- Instead of implementing multiple solvers,
-- we use the single solver implemented for the solution.
-- We also include Sudoku puzzles which are known to have solutions,
-- in case the solver only generates pathological test cases
-- where they are the only ones the solver knows how to solve.
instance Arbitrary SudokuPuzzle where
  arbitrary = frequency [ (10, puzzles), (1, elements knownSudokuPuzzles) ]

-- | Generates an arbitrary Sudoku puzzle with a solution.
--
-- Test case size controls number of spots to make blank.
puzzles :: Gen SudokuPuzzle
puzzles = MkGen gen
  where gen g n = SudokuPuzzle $ makeBlankSpots g' n randomSolution
          where randomSolution = fromJust $ fst $ randomSudoku empty g''
                empty = replicate 9 $ replicate 9 0
                randomSudoku = Solution.randomSudoku
                (g',g'') = split g

makeBlankSpots :: RandomGen g => g -> Int -> [[Int]] -> [[Int]]
makeBlankSpots g n p = fst $ iterate step (p, positions) !! n
  where step (p', positions') = (remove (head positions') p', tail positions')
        positions = zip (randomRs (1,9) g') (randomRs (1,9) g'')  -- random positions
        remove (x,y) p' = take (x-1) p' ++ [removey y $ p' !! (x-1)] ++ drop x p'
        removey y ns = take (y-1) ns ++ [0] ++ drop y ns
        (g',g'') = split g

-- Sudoku puzzles which are known to have solutions.
knownSudokuPuzzles :: [SudokuPuzzle]
knownSudokuPuzzles = map SudokuPuzzle
  [ [ [ 0, 6, 0, 1, 0, 8, 0, 0, 0 ]
    , [ 1, 0, 0, 0, 0, 4, 0, 0, 5 ]
    , [ 0, 4, 0, 3, 9, 0, 0, 7, 0 ]
    , [ 0, 0, 4, 0, 7, 1, 0, 9, 8 ]
    , [ 8, 1, 0, 0, 6, 0, 0, 5, 2 ]
    , [ 6, 9, 0, 4, 8, 0, 7, 0, 0 ]
    , [ 0, 2, 0, 0, 1, 7, 0, 3, 0 ]
    , [ 4, 0, 0, 8, 0, 0, 0, 0, 6 ]
    , [ 0, 0, 0, 5, 0, 2, 0, 8, 0 ]
    ]
  , [ [ 0, 0, 3, 0, 0, 0, 7, 0, 0 ]
    , [ 0, 2, 0, 5, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 7, 0, 8, 1, 0 ]
    , [ 0, 0, 0, 0, 0, 4, 0, 0, 0 ]
    , [ 6, 7, 0, 0, 0, 0, 9, 0, 0 ]
    , [ 9, 0, 0, 7, 5, 0, 0, 0, 6 ]
    , [ 5, 6, 0, 0, 0, 0, 0, 4, 0 ]
    , [ 0, 0, 0, 2, 0, 0, 0, 0, 7 ]
    , [ 3, 4, 0, 1, 0, 0, 0, 0, 5 ]
    ]
  , [ [ 0, 0, 0, 0, 2, 0, 0, 0, 7 ]
    , [ 0, 7, 0, 0, 0, 1, 0, 0, 0 ]
    , [ 9, 0, 0, 0, 0, 0, 0, 5, 6 ]
    , [ 0, 9, 0, 8, 3, 0, 0, 0, 2 ]
    , [ 0, 0, 0, 0, 4, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 6, 0, 2, 0, 0, 0 ]
    , [ 0, 3, 0, 0, 7, 6, 0, 0, 0 ]
    , [ 0, 0, 6, 5, 0, 0, 9, 0, 0 ]
    , [ 0, 2, 0, 0, 0, 0, 0, 0, 3 ]
    ]
  , [ [ 0, 0, 0, 5, 1, 8, 0, 0, 0 ]
    , [ 1, 0, 0, 2, 0, 6, 0, 0, 5 ]
    , [ 3, 2, 0, 0, 0, 0, 0, 8, 1 ]
    , [ 4, 6, 0, 0, 9, 0, 0, 3, 8 ]
    , [ 8, 0, 0, 3, 0, 5, 0, 0, 4 ]
    , [ 5, 3, 0, 0, 2, 0, 0, 6, 9 ]
    , [ 7, 8, 0, 0, 0, 0, 0, 4, 2 ]
    , [ 9, 0, 0, 4, 0, 2, 0, 0, 3 ]
    , [ 0, 0, 0, 1, 7, 9, 0, 0, 0 ]
    ]
  , [ [ 0, 0, 0, 4, 3, 0, 0, 5, 9 ]
    , [ 0, 2, 5, 0, 0, 8, 0, 7, 0 ]
    , [ 7, 0, 3, 2, 0, 0, 6, 0, 0 ]
    , [ 2, 0, 0, 7, 9, 0, 3, 8, 0 ]
    , [ 8, 4, 0, 1, 2, 0, 0, 0, 0 ]
    , [ 1, 3, 0, 0, 8, 0, 4, 9, 0 ]
    , [ 0, 0, 0, 8, 0, 1, 0, 0, 6 ]
    , [ 0, 0, 2, 0, 0, 9, 8, 0, 4 ]
    , [ 9, 1, 8, 0, 0, 2, 0, 0, 5 ]
    ]
  ]
