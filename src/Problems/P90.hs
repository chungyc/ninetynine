{- |
Description: `queens`

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P90".
-}
module Problems.P90 (queens) where

import qualified Solutions.P90 as Solution

-- $setup
-- >>> import Data.List

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
--
-- === Examples
--
-- >>> length (queens 8)
-- 92
--
-- >>> head $ sort $ queens 8
-- [1,5,8,6,3,7,2,4]
queens :: Int -> [[Int]]
queens = Solution.queens
