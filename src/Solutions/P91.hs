{- |
Description: Knight's tour
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P91" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P91 (knightsTour,closedKnightsTour) where

import           Data.List  (sortOn)
import           Data.Maybe (isJust)
import           Data.Set   (Set)
import qualified Data.Set   as Set

-- | Returns a knight's tour ending at a particular square.
-- Represents the squares by their coordinates with
-- the tuple \((x,y)\), where \(1 \leq x \leq N\) and \(1 \leq y \leq N\).
-- A tour will be a list of these tuples of length \(N \times N\).
knightsTour :: Int -> (Int,Int) -> Maybe [(Int,Int)]
knightsTour n pos
  | not legalEnd = Nothing
  | otherwise    = tour n [pos] $ Set.fromList [(x,y) | x <- [1..n], y <- [1..n], (x,y) /= pos]
  where legalEnd = isLegalPosition n pos

tour :: Int -> [(Int,Int)] -> Set (Int,Int) -> Maybe [(Int,Int)]
tour n path remaining
  | Set.null remaining = Just path
  | null next          = Nothing
  | otherwise          = path'
  where path' | null paths = Nothing
              | otherwise  = head paths
        paths = filter isJust $ map (\pos -> tour n (pos:path) $ Set.delete pos remaining) next
        -- Apply Warnsdorff's heuristic.
        next = sortOn (\pos -> availableMoves (Set.delete pos remaining) pos) $ nextMoves remaining $ head path

nextMoves :: Set (Int,Int) -> (Int,Int) -> [(Int,Int)]
nextMoves remaining (x,y) = filter (\pos -> Set.member pos remaining) $
  [(x+xd,y+yd) | xd <- [1,-1], yd <- [2,-2]] ++ [(x+xd,y+yd) | xd <- [2,-2], yd <- [1,-1]]

availableMoves :: Set (Int,Int) -> (Int,Int) -> Int
availableMoves remaining pos = length $ nextMoves remaining pos

isLegalPosition :: Int -> (Int,Int) -> Bool
isLegalPosition n (x,y) = inRange x && inRange y
  where inRange z = 1 <= z && z <= n

-- | The same as 'knightsTour', except return a circular tour.
-- I.e., the knight must be able to jump from the last position in the tour to the first position in the tour.
-- Starts the tour from \((1,1)\).
closedKnightsTour :: Int -> Maybe [(Int,Int)]
closedKnightsTour n
  | n < 3  = Nothing  -- Not enough room to jump anywhere.
  -- Only tours which end at (2,3) or (3,2) can move back to (1,1).
  -- Arbitrarily end at (2,3).
  | otherwise = closedTour n [(2,3)] $ Set.fromList [(x,y) | x <- [1..n], y <- [1..n], (x,y) /= (2,3)]

closedTour :: Int -> [(Int,Int)] -> Set (Int,Int) -> Maybe [(Int,Int)]
closedTour n path remaining
  | Set.null remaining = if head path == (1,1) then Just path else Nothing
  | null next          = Nothing
  | otherwise          = path'
  where path' | null paths = Nothing
              | otherwise  = head paths
        paths = filter isJust $ map (\pos -> tour n (pos:path) $ Set.delete pos remaining) next
        next = nextMoves remaining $ head path
