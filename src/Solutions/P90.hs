{- |
Description: 'queens'
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P90" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P90 (queens) where

-- | Find all solutions to the \(n\) queens problem.
--
-- The eight queens problem is a classical problem in computer science.
-- The objective is to place eight queens on a chessboard so that no two queens are attacking each other.
-- I.e., no two queens are in the same row, the same column, or on the same diagonal.
-- Solve the extended problem for arbitrary \(n\).
--
-- Represent the positions of the queens as a list of numbers from 1 to \(n\).
-- For example, @[4,2,7,3,6,8,5,1]@ means that the queen in the first column is in row 4,
-- the queen in the second column is in row 2, etc.
queens :: Int -> [[Int]]
queens n = enumerate [] 1 [1..n]

enumerate :: [(Int, Int)] -> Int -> [Int] -> [[Int]]
enumerate assigned _ [] = [map snd assigned]
enumerate assigned column remainingRows = concat $ map expand possibleRows
  where expand r = enumerate ((column, r) : assigned) (column+1) (rowExcluded r)
        rowExcluded r = filter (r /=) remainingRows
        possibleRows = filter (not . (attacksPrevious assigned column)) remainingRows

attacksPrevious :: [(Int, Int)] -> Int -> Int -> Bool
attacksPrevious assigned column row = any attacks assigned
  -- Due to how rows are extracted to be assigned at each column,
  -- no queen can attack another via row or column.
  where attacks (c, r) = abs (column - c) == abs (row - r)
