{- |
Description: Nonograms
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P98".
-}
module Problems.P98 (nonogram, printNonogramPuzzle, printNonogramSolution, nonogramPuzzle) where

import           Data.List     (group, transpose)
import qualified Solutions.P98 as Solution

{- |
[Nonograms](https://en.wikipedia.org/wiki/Nonogram) are picture logic puzzles invented in 1987,
spreading from Japan to across the world.  They look like this:

@
 □ □ □ □ □ □ □ □  3
 □ □ □ □ □ □ □ □  2 1
 □ □ □ □ □ □ □ □  3 2
 □ □ □ □ □ □ □ □  2 2
 □ □ □ □ □ □ □ □  6
 □ □ □ □ □ □ □ □  1 5
 □ □ □ □ □ □ □ □  6
 □ □ □ □ □ □ □ □  1
 □ □ □ □ □ □ □ □  2

 1 3 1 7 5 3 4 3

 2 1 5 1
@

Essentially, each row and column of a rectangular bitmap is annotated
with the respective lengths of its distinct strings of occupied cells.
The person who solves the puzzle must complete the bitmap given only these lengths.
Published puzzles are larger than this example, e.g., \(25 \times 20\),
and apparently always have unique solutions.

Try to solve the \(25 \times 25\) puzzle in 'nonogramPuzzle'.

=== Examples

>>> :{
printNonogramSolution $
nonogram [[3],[2,1],[3,2],[2,2],[6],[1,5],[6],[1],[2]]
         [[1,2],[3,1],[1,5],[7,1],[5],[3],[4],[3]]
:}
   ■ ■ ■          3
 ■ ■   ■          2 1
   ■ ■ ■     ■ ■  3 2
     ■ ■     ■ ■  2 2
     ■ ■ ■ ■ ■ ■  6
 ■   ■ ■ ■ ■ ■    1 5
 ■ ■ ■ ■ ■ ■      6
         ■        1
       ■ ■        2
<BLANKLINE>
 1 3 1 7 5 3 4 3
<BLANKLINE>
 2 1 5 1

=== __Hint__

If there is a string of occupied cells with length 9 in a row of 10 cells,
can we infer which cells in the row /must/ be occupied?
-}
nonogram :: [[Int]]  -- ^ Lengths of occupied cells in each row
         -> [[Int]]  -- ^ Lengths of occupied cells in each column
         -> Maybe [[Bool]]  -- ^ Solution to the puzzle, if it exists
nonogram = Solution.nonogram

{- |
Print out a nonogram puzzle.

>>> :{
printNonogramPuzzle
  [[3],[2,1],[3,2],[2,2],[6],[1,5],[6],[1],[2]]
  [[1,2],[3,1],[1,5],[7,1],[5],[3],[4],[3]]
:}
 □ □ □ □ □ □ □ □  3
 □ □ □ □ □ □ □ □  2 1
 □ □ □ □ □ □ □ □  3 2
 □ □ □ □ □ □ □ □  2 2
 □ □ □ □ □ □ □ □  6
 □ □ □ □ □ □ □ □  1 5
 □ □ □ □ □ □ □ □  6
 □ □ □ □ □ □ □ □  1
 □ □ □ □ □ □ □ □  2
<BLANKLINE>
 1 3 1 7 5 3 4 3
<BLANKLINE>
 2 1 5 1
-}
printNonogramPuzzle :: [[Int]] -> [[Int]] -> IO ()
printNonogramPuzzle rows columns = printNonogram rows columns Nothing

{- |
Print out a nonogram solution.

>>> :{
printNonogramSolution $
nonogram [[3],[2,1],[3,2],[2,2],[6],[1,5],[6],[1],[2]]
         [[1,2],[3,1],[1,5],[7,1],[5],[3],[4],[3]]
:}
   ■ ■ ■          3
 ■ ■   ■          2 1
   ■ ■ ■     ■ ■  3 2
     ■ ■     ■ ■  2 2
     ■ ■ ■ ■ ■ ■  6
 ■   ■ ■ ■ ■ ■    1 5
 ■ ■ ■ ■ ■ ■      6
         ■        1
       ■ ■        2
<BLANKLINE>
 1 3 1 7 5 3 4 3
<BLANKLINE>
 2 1 5 1
-}
printNonogramSolution :: Maybe [[Bool]] -> IO ()
printNonogramSolution Nothing  = return ()
printNonogramSolution (Just p) = printNonogram rows columns $ Just $ map (map Just) p
  where rows = getLengths p
        columns = getLengths $ transpose p

getLengths :: [[Bool]] -> [[Int]]
getLengths []      = []
getLengths picture = map (map length . filter head . group) picture

-- | Print out a nonogram puzzle or solution.
printNonogram :: [[Int]] -> [[Int]] -> Maybe [[Maybe Bool]] -> IO ()
printNonogram rows columns Nothing =
  printNonogram rows columns $ Just $ replicate (length rows) $ replicate (length columns) Nothing
printNonogram [] columns (Just []) = do
  putStrLn ""
  printColumns columns
printNonogram (r:rows) columns (Just (c:cells)) = do
  printRowCells c
  putStr " "
  printRowLengths r
  putStrLn ""
  printNonogram rows columns $ Just cells
printNonogram _ _ _ = undefined

printRowCells :: [Maybe Bool] -> IO ()
printRowCells = mapM_ (putStr . format)
  where format Nothing      = " □"
        format (Just False) = "  "
        format (Just True)  = " ■"

printRowLengths :: [Int] -> IO ()
printRowLengths = mapM_ (putStr . (' ':) . show)

printColumns :: [[Int]] -> IO ()
printColumns columns = mapM_ printLine ls
  where ls = formatColumns columns
        printLine l = do
          mapM_ (\x -> putStr $ ' ':[x]) l
          putStrLn ""

formatColumns :: [[Int]] -> [String]
formatColumns columns = transpose texts'
  where texts = map (unwords . map show) columns
        -- make all strings the same length
        texts' = map (\t -> t ++ replicate (l - length t) ' ') texts
        l = maximum $ map length texts

-- | A nonogram puzzle of size \(25 \times 25\).
--
-- >>> printNonogramSolution $ let (rs, cs) = nonogramPuzzle in nonogram rs cs
-- ...
nonogramPuzzle :: ([[Int]],[[Int]])
nonogramPuzzle =
  -- From https://nonograms-katana.com/, which states "All puzzles are free".
  -- This is the 25x25 Pegasus puzzle.
  ( [ [11], [11], [10], [9,1], [8,3], [8,5], [7,7], [7,9], [5,5,2], [9], [13], [16], [2,14], [2,14]
    , [3,14], [2,15], [2,4,5], [3,4,3], [2,5,1,1], [2,2,1,1], [1,2,2,1], [1,2,2], [1,1], [2,2], [2,2]
    ]
  , [ [1,2], [1,5], [2,6], [2,4], [3,1], [3,5,6], [4,10,2], [6,9,1], [8,11], [9,12], [9,6,3]
    , [15,2], [15,1], [14], [5,7], [8], [10], [11], [12,1], [6,6], [4,2,1], [5,5], [3], [3], [2]
    ]
  )
