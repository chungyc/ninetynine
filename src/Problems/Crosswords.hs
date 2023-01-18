{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{- |
Description: Supporting definitions for crossword puzzles
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Supporting definitions for crossword puzzles.
In particular, this supports "Problems.P99".
-}
module Problems.Crosswords (Crossword (..), printCrossword, parseCrossword, readCrossword) where

import           Data.List (intersperse)
import           System.IO

{- |
A crossword puzzle.

A list of words to fill the puzzle with is given along the grid to fill.
The crossword puzzle grid is represented with a list of sublists.
Each sublist denotes a row, and each value in the sublists denotes a spot.
For each value in a spot:

* 'True' denotes a blank spot that needs a character to be filled in.
* 'False' denotes a spot that cannot be filled in.
*  A character value denotes a spot pre-filled with the character.
-}
data Crossword = Crossword
  { word :: [String]              -- ^ List of words to fill crossword puzzle with
  , grid :: [[Either Bool Char]]  -- ^ Grid for the crossword puzzle
  }
  deriving (Eq, Show)

{- |
Print out a solution to a crossword puzzle.

>>> :{
printCrossword $ Just [ [ Nothing,  Nothing,  Just 'P', Nothing,  Nothing  ]
                      , [ Nothing,  Nothing,  Just 'O', Nothing,  Nothing  ]
                      , [ Just 'A', Just 'L', Just 'P', Just 'H', Just 'A' ]
                      , [ Nothing,  Nothing,  Just 'P', Nothing,  Just 'R' ]
                      , [ Nothing,  Nothing,  Just 'Y', Nothing,  Just 'E' ]
                      , [ Nothing,  Nothing,  Nothing,  Nothing,  Just 'S' ]
                      ]
:}
■ ■ P ■ ■
■ ■ O ■ ■
A L P H A
■ ■ P ■ R
■ ■ Y ■ E
■ ■ ■ ■ S
-}
printCrossword :: Maybe [[Maybe Char]] -> IO ()
printCrossword Nothing = return ()
printCrossword (Just solution) = mapM_ printRow solution
  where printRow cs = putStrLn $ intersperse ' ' $ map fromSpot cs
        fromSpot Nothing  = '■'
        fromSpot (Just c) = c

{- |
Parses a crossword puzzle specification in a particular syntax.

It first lists the words in an arbitrary order, one word per line.
Then, after an empty line, the crossword grid is defined.
In this grid specification, a blank spot is represented by a dot (@.@).
Spots can also contain predefined character values.

=== __Notes__

This parses the crossword specifications provided by problem 99 in the original list.
-}
parseCrossword :: String -> Maybe Crossword
parseCrossword spec
  | null gridLines' = Nothing
  | otherwise = Just $ Crossword { word = wordLines, grid = parseGrid gridLines }
  where (wordLines, gridLines') = break ("" ==) $ lines spec
        _ : gridLines = gridLines'

parseGrid :: [String] -> [[Either Bool Char]]
parseGrid ls = padRows $ map parseLine ls
  where parseLine = map parseSpot
        parseSpot ' ' = Left False
        parseSpot '.' = Left True
        parseSpot c   = Right c

padRows :: [[Either Bool Char]] -> [[Either Bool Char]]
padRows rs = map pad rs
  where n = maximum $ map length rs
        pad r = r ++ replicate (n - length r) (Left False)

-- | Reads a crossword puzzle from a file, whose syntax is as specified by 'parseCrossword'.
--
-- === __Notes__
--
-- This parses the crossword specifications provided by problem 99 in the original list.
readCrossword :: FilePath -> IO (Maybe Crossword)
readCrossword path = do
  h <- openFile path ReadMode
  spec <- hGetContents h
  return $ parseCrossword spec
