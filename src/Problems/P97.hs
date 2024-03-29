{- |
Description: Sudoku
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P97".
-}
module Problems.P97 (sudoku, sudokuPuzzle, printSudoku) where

import qualified Solutions.P97 as Solution

{- |
A Sudoku puzzle looks like this:

\[
\begin{array}{|ccc|ccc|ccc|}
\hline
. & . & 4 & 8 & . & . & . & 1 & 7 \\
6 & 7 & . & 9 & . & . & . & . & . \\
5 & . & 8 & . & 3 & . & . & 2 & 4 \\
\hline
3 & . & . & 7 & 4 & . & 1 & . & . \\
. & 6 & 9 & . & . & . & 7 & 8 & . \\
. & . & 1 & . & 6 & 9 & . & . & 5 \\
\hline
1 & . & . & . & 8 & . & 3 & . & . \\
. & . & . & . & . & 6 & . & 9 & 1 \\
2 & 4 & . & . & . & 1 & 5 & . & . \\
\hline
\end{array}
\]

The solution for the above is:

\[
\begin{array}{|ccc|ccc|ccc|}
\hline
9 & 3 & 4 & 8 & 2 & 5 & 6 & 1 & 7 \\
6 & 7 & 2 & 9 & 1 & 4 & 8 & 5 & 3 \\
5 & 1 & 8 & 6 & 3 & 7 & 9 & 2 & 4 \\
\hline
3 & 2 & 5 & 7 & 4 & 8 & 1 & 6 & 9 \\
4 & 6 & 9 & 1 & 5 & 3 & 7 & 8 & 2 \\
7 & 8 & 1 & 2 & 6 & 9 & 4 & 3 & 5 \\
\hline
1 & 9 & 7 & 5 & 8 & 2 & 3 & 4 & 6 \\
8 & 5 & 3 & 4 & 7 & 6 & 2 & 9 & 1 \\
2 & 4 & 6 & 3 & 9 & 1 & 5 & 7 & 8 \\
\hline
\end{array}
\]

Every spot in the puzzle on a \(9 \times 9\) board belongs to a row and a column,
as well as to one single \(3 \times 3\) square, which we will simply call "square" for short.
At the beginning, some of the spots carry a single-digit number from 1 to 9.
The problem is to fill the missing spots with digits in such a way that every number
between 1 and 9 appears exactly once in each row, in each column, and in each square.

Write a function which returns a solution given a Sudoku puzzle.
Both will be expressed as a list of 9 rows.
Each row will be a list of 9 numbers from 0 to 9, where 0 signifies a blank spot.

=== Examples

>>> printSudoku $ sudoku sudokuPuzzle
9 3 4 8 2 5 6 1 7
6 7 2 9 1 4 8 5 3
5 1 8 6 3 7 9 2 4
3 2 5 7 4 8 1 6 9
4 6 9 1 5 3 7 8 2
7 8 1 2 6 9 4 3 5
1 9 7 5 8 2 3 4 6
8 5 3 4 7 6 2 9 1
2 4 6 3 9 1 5 7 8
-}
sudoku :: [[Int]] -> Maybe [[Int]]
sudoku = Solution.sudoku

-- | An example Sudoku puzzle which can be fed into 'sudoku'.
sudokuPuzzle :: [[Int]]
sudokuPuzzle =
  [ [ 0, 0, 4, 8, 0, 0, 0, 1, 7 ]
  , [ 6, 7, 0, 9, 0, 0, 0, 0, 0 ]
  , [ 5, 0, 8, 0, 3, 0, 0, 2, 4 ]
  , [ 3, 0, 0, 7, 4, 0, 1, 0, 0 ]
  , [ 0, 6, 9, 0, 0, 0, 7, 8, 0 ]
  , [ 0, 0, 1, 0, 6, 9, 0, 0, 5 ]
  , [ 1, 0, 0, 0, 8, 0, 3, 0, 0 ]
  , [ 0, 0, 0, 0, 0, 6, 0, 9, 1 ]
  , [ 2, 4, 0, 0, 0, 1, 5, 0, 0 ]
  ]

-- | Prints a puzzle or solution for 'sudoku' in tabular form.
printSudoku :: Maybe [[Int]] -> IO ()
printSudoku Nothing = return ()
printSudoku (Just solution) = mapM_ putStrLn outputLines
  where outputLines = map (unwords . map showInt) solution
        showInt 0 = "."
        showInt n = show n
