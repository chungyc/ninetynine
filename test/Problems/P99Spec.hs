{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P99Spec (spec) where

import           Data.List             (nub, transpose, zip4)
import           Data.Maybe            (fromJust, isJust, isNothing)
import           Data.Set              (Set, isSubsetOf)
import qualified Data.Set              as Set
import           Problems.Crosswords
import qualified Problems.P99          as Problem
import qualified Solutions.P99         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Crossword -> Maybe [[Maybe Char]]) -> String -> Spec
properties solveCrossword name = describe name $ modifyMaxSize (const 50) $ do
  prop "fills given grid" $
    \(Puzzle p) -> solveCrossword p `shouldSatisfy` isGridFilled (grid p)

  prop "uses only given words in rows" $
    \(Puzzle p) -> solveCrossword p `shouldSatisfy`
                   \(Just s) -> all (includesGivenWords $ word p) s

  prop "uses only given words in columns" $
    \(Puzzle p) -> solveCrossword p `shouldSatisfy`
                   \(Just s) -> all (includesGivenWords $ word p) (transpose s)

examples :: Spec
examples = describe "Examples" $ do
  it "solveCrossword $ Crossword { word = [\"ALPHA\", ..." $ do
    solveCrossword (Crossword
      { word = ["ALPHA", "ARES", "POPPY"]
      , grid = [ [ Left False, Left False, Left True,  Left False, Left False ]
               , [ Left False, Left False, Left True,  Left False, Left False ]
               , [ Left True,  Left True,  Left True,  Left True,  Left True  ]
               , [ Left False, Left False, Left True,  Left False, Left True  ]
               , [ Left False, Left False, Left True,  Left False, Left True  ]
               , [ Left False, Left False, Left False, Left False, Left True  ]
               ]
      })
      `shouldBe`
      Just [ [ Nothing,  Nothing,  Just 'P', Nothing,  Nothing  ]
           , [ Nothing,  Nothing,  Just 'O', Nothing,  Nothing  ]
           , [ Just 'A', Just 'L', Just 'P', Just 'H', Just 'A' ]
           , [ Nothing,  Nothing,  Just 'P', Nothing,  Just 'R' ]
           , [ Nothing,  Nothing,  Just 'Y', Nothing,  Just 'E' ]
           , [ Nothing,  Nothing,  Nothing,  Nothing,  Just 'S' ]
           ]

  where solveCrossword = Problem.solveCrossword

spec :: Spec
spec = parallel $ do
  properties Problem.solveCrossword "solveCrossword"
  examples
  describe "From solutions" $ do
    properties Solution.solveCrossword "solveCrossword"

isGridFilled :: [[Either Bool Char]] -> Maybe [[Maybe Char]] -> Bool
isGridFilled _ Nothing            = False
isGridFilled g (Just s)
  | not sameDimensions = False
  | otherwise          = all (uncurry isRowFilled) $ zip g s
  where sameDimensions = map length g == map length s
        isRowFilled row spots = all (uncurry isSpotFilled) $ zip row spots
        isSpotFilled (Left False) Nothing = True
        isSpotFilled (Left False) _       = False
        isSpotFilled (Left True) Nothing  = False
        isSpotFilled (Left True) _        = True
        isSpotFilled (Right _) Nothing    = False
        isSpotFilled (Right c) (Just c')  = c == c'

includesGivenWords :: [String] -> [Maybe Char] -> Bool
includesGivenWords ws r = extractWords r `isSubsetOf` Set.fromList ws

extractWords :: [Maybe Char] -> Set String
extractWords cs
  | null cs'       = Set.empty
  | length str > 1 = Set.insert str $ extractWords cs''
  | otherwise      = extractWords cs''
  where cs' = dropWhile isNothing cs
        (candidate, cs'') = span isJust cs'
        str = map fromJust candidate

newtype Puzzle = Puzzle Crossword deriving Show

instance Arbitrary Puzzle where
  arbitrary = sized $ \n -> genPuzzle (n+2)

genPuzzle :: Int -> Gen Puzzle
genPuzzle n = do
  m <- chooseInt (1,n*n)
  ws <- vectorOf m $ resize m $ suchThat (listOf1 $ elements ['a'..'z']) ((<) 1 . length)
  xs <- vectorOf m $ chooseInt (1,n)
  ys <- vectorOf m $ chooseInt (1,n)
  zs <- vectorOf m $ choose (False,True)
  mask <- vectorOf n $ vectorOf n $ frequency [(10, return False), (1, return True)]
  let s = constructSolution blank $ zip4 ws xs ys zs
  let c = Crossword { word = getWords s, grid = getGrid s mask }
  case nub (word c) == word c of
    True  -> return $ Puzzle c
    False -> genPuzzle n  -- a word used twice; try again
  where blank = replicate n $ replicate n Nothing

-- | Construct a solution from the given words and their locations and orientations.
--
-- zs are orientations; False -> horizontal, True -> vertical
constructSolution :: [[Maybe Char]] -> [(String,Int,Int,Bool)] -> [[Maybe Char]]
constructSolution g []        = g
constructSolution g (w:ws)
  | isValid g' = constructSolution g' ws
  | otherwise  = constructSolution g ws
  where g' = addToSolution g w

-- | Add word at given location and orientation to solution.
addToSolution :: [[Maybe Char]] -> (String,Int,Int,Bool) -> [[Maybe Char]]
addToSolution g (w,x,y,True) = transpose $ addToSolution (transpose g) (w,y,x,False)
addToSolution g (w,x,y,False)
  | x + length w > length g = g
  | otherwise               = g'
  where front = take (y-1) g
        row = head $ drop (y-1) g
        row' = take x row ++ map Just w ++ drop (x + length w) row
        back = drop y g
        g' = front ++ [row'] ++ back

-- We like crossings, but we don't want entire blank areas of blank spots.
-- I.e., we don't like sites that we didn't add ourselves.
-- Reject grids which have 2x2 blank areas.
isValid :: [[Maybe Char]] -> Bool
isValid g = all isValidRows $ zip g $ tail g
  where isValidRows ([], []) = True
        isValidRows (Just _ : Just _ : _,
                     Just _ : Just _ : _) = False
        isValidRows (_:xs, _:ys) = isValidRows (xs,ys)
        isValidRows _ = undefined

getWords :: [[Maybe Char]] -> [String]
getWords g = horizontal ++ vertical
  where horizontal = Set.toList $ Set.unions $ map extractWords g
        vertical = Set.toList $ Set.unions $ map extractWords $ transpose g

getGrid :: [[Maybe Char]] -> [[Bool]] -> [[Either Bool Char]]
getGrid g mask = map toRow $ zip g mask
  where toRow (r, m) = map toSpot $ zip r m
        toSpot (Nothing, _)    = Left False
        toSpot (Just _, False) = Left True
        toSpot (Just c, True)  = Right c
