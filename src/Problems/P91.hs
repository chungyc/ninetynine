{- |
Description: Knight's tour
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P91".
-}
module Problems.P91 (knightsTour, closedKnightsTour, printKnightsTour) where

import           Data.List       (intercalate)
import qualified Data.Map.Strict as Map
import qualified Solutions.P91   as Solution

-- | Another famous problem is this one:
-- How can a knight jump on an \(N \times N\) chessboard in such a way that it visits every square exactly once?
--
-- Write a function which returns a knight's tour ending at a particular square.
-- Represent the squares by their coordinates with
-- the tuple \((x,y)\), where \(1 \leq x \leq N\) and \(1 \leq y \leq N\).
-- A tour will be a list of these tuples of length \(N * N\).
--
-- === Examples
--
-- >>> printKnightsTour $ knightsTour 6 (3,5)
-- 24  7 32 17 22  5
-- 33 16 23  6 31 18
--  8 25 10 19  4 21
-- 15 34  1 28 11 30
-- 26  9 36 13 20  3
-- 35 14 27  2 29 12
--
-- === __Hint__
--
-- A straightforward backtracking algorithm can be very slow even for
-- moderately sized boards such as \(8 \times 8\).
-- Consider using [Warnsdorff's rule](https://en.wikipedia.org/wiki/Knight%27s_tour#Warnsdorff's_rule).
-- Alternatively, consider using a divide-and conquer algorithm which
-- finds knight's tours for smaller boards and patching them together.
knightsTour :: Int -> (Int,Int) -> Maybe [(Int,Int)]
knightsTour = Solution.knightsTour

-- | The same as 'knightsTour', except return a circular tour.
-- I.e., the knight must be able to jump from the last position in the tour to the first position in the tour.
-- Start the tour from \((1,1)\).
--
-- === Examples
--
-- >>> printKnightsTour $ closedKnightsTour 6
--  1 14 31 20  3  8
-- 32 21  2  7 30 19
-- 13 36 15  4  9  6
-- 22 33 24 27 18 29
-- 25 12 35 16  5 10
-- 34 23 26 11 28 17
closedKnightsTour :: Int -> Maybe [(Int,Int)]
closedKnightsTour = Solution.closedKnightsTour

-- | Print order of knight's tour on an \(N \times N\) board.
printKnightsTour :: Maybe [(Int,Int)] -> IO ()
printKnightsTour Nothing = return ()
printKnightsTour (Just path) = mapM_ (\y -> putStrLn $ line y) [1..n]
  where order = Map.fromList $ zip path [1..(n*n)]
        line y = intercalate " " $ map showInt $ map (\x -> order Map.! (x,y)) [1..n]
        showInt k = replicate (width - (length $ show k)) ' ' ++ (show k)
        width = length (show $ n*n)
        l = length path
        n = head $ takeWhile (\k -> k*k == l) $ dropWhile (\k -> k*k < l) [1..]
