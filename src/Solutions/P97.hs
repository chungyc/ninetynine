{- |
Description: Sudoku
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P97" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P97 (sudoku, randomSudoku) where

import           Data.Array    (Array, array)
import qualified Data.Array    as Array
import           Data.Ix       (inRange)
import           Data.List     (sortOn)
import qualified Data.List     as List
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import           Data.Maybe    (isNothing, mapMaybe)
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           System.Random

{- | Returns a solution for a given Sudoku puzzle.

Both will be expressed as a list of 9 rows.
Each row will be a list of 9 numbers from 0 to 9, where 0 signifies a blank spot.
-}
sudoku :: [[Int]] -> Maybe [[Int]]
sudoku puzzle = fst $ randomSudoku puzzle gen
  where gen = mkStdGen 111111  -- arbitrary but deterministic

-- | Solves Sudoku puzzles.
--
-- Uses the given source of randomness when choosing among multiple possibilities.
-- This underlies the 'sudoku' function, which uses a fixed source of randomness.
--
-- The overall approach is thus:
--
-- 1. Prune possible values for blank spots so that values which are not possible
--    are pruned.  This is determined by what numbers have been definitely determined
--    for other positions in the same row, column, and square.
--
--      * Some blank spots may end up with a single possible value.
--        In this case, it has become a position with a definite value.
--
-- 2. Repeat pruning until there are no more possibilities to be pruned.
--    If all positions have definite values, we have found a solution.
--
-- 3. If we can't prune enough possibilities to get a solution,
--    pick a random position.  For each of its possible values,
--    pretend it is definite, i.e., part of the solution,
--    and repeat the pruning from step 1.
randomSudoku :: RandomGen g => [[Int]] -> g -> (Maybe [[Int]], g)
randomSudoku puzzle gen
  | valid     = (fmap fromBoard solution, gen')
  | otherwise = (Nothing, gen)
  where (solution, gen') = solve board pending gen
        (board, pending) = toBoard puzzle
        valid = validNumbers && validSize && validateConflicts board pending
        validSize = (length puzzle == 9) && all ((9 ==) . length) puzzle
        validNumbers = all (all $ inRange (0,9)) puzzle

-- | Stores the definite value for each position that has one.
type Board = Array (Int,Int) (Maybe Int)

-- | Maps positions to its multiple possible values.
-- Intended to hold positions that are still blank, not all positions.
type Pending = Map (Int,Int) (Set Int)

-- | Converts a puzzle or solution represented as a Board into a list of lists.
fromBoard :: Board -> [[Int]]
fromBoard b = [ l | i <- [1..9], let l = [ toInt $ b Array.! (i,j) | j <- [1..9] ] ]
  where toInt Nothing  = 0
        toInt (Just n) = n

-- | Turn the list of lists into a form more convenient to finding a solution.
toBoard :: [[Int]] -> (Board, Pending)
toBoard p = (array ((1,1), (9,9)) list, toPending list)
  where list = toBoardList p

-- | Converts to association list for use by toBoard.
toBoardList :: [[Int]] -> [((Int, Int), Maybe Int)]
toBoardList p = concatMap (\(i, l) -> map (\(j, n) -> ((i, j), maybeN n)) l) indexed
  where indexed = zip [1..9] $ map (zip [1..9]) p
        maybeN 0 = Nothing
        maybeN n = Just n

-- | Fill in the possibilities for blank spots.
toPending :: [((Int, Int), Maybe Int)] -> Pending
toPending list = Map.fromList $ map (\(pos, _) -> (pos, Set.fromList [1..9])) positions
  where positions = filter (\(_, n) -> isNothing n) list

-- | Given a board with definite values and the possible values for blank spots,
-- search for a solution to the puzzle.
--
-- This is the high-level function doing the main work of finding a solution.
solve :: RandomGen g => Board -> Pending -> g -> (Maybe Board, g)
solve b p gen
  | isImpossible   = (Nothing, gen)
  | isSolution     = (Just $ fst $ incorporatePending b p, gen)
  -- Prune until there is nothing more to prune.
  | pruned         = solve b p' gen
  -- We have reached a point where we cannot definitely prune possibilities.
  -- Try guessing which possibilities lead to a solution.
  | otherwise      = guess b p' gen
  where (p', pruned) = prune b p
        isSolution = all ((==) 1 . Set.size) p
        isImpossible = any Set.null p

-- | Verifies that no positions with a definite value are inconsistent with a solution.
-- I.e., that there are no other positions in the same row, column, or square with the same definite value.
validateConflicts :: Board -> Pending -> Bool
validateConflicts b p = all (\e -> validatePosition e $ b Array.! e) $ Array.indices b
  where validatePosition _ Nothing  = True
        validatePosition e (Just n) = not $ Set.member n $ conflicting b p e

-- | Incorporate pending positions with definite values into the board itself.
-- Returns the resulting board and the pending positions which remain.
incorporatePending :: Board -> Pending -> (Board, Pending)
incorporatePending b p = (b Array.// assocs', p')
  where (assocs', p') = Map.foldlWithKey incorporate ([], p) p
        incorporate (assocs'', p'') e cs
          | Set.size cs == 1 = ((e, Just $ Set.findMin cs) : assocs'', Map.delete e p'')
          | otherwise        = (assocs'', p'')

-- | Prune possible values which cannot lead to a solution.
-- I.e., values which already exist in the same row, column, or square.
--
-- Also returns a boolean value signaling whether anything has been pruned.
prune :: Board -> Pending -> (Pending, Bool)
prune b p = Map.foldlWithKey step (p, False) p
  where step s@(p', _) e cs = merge s $ prunePosition b p' e cs
        merge (_, changed) (p', changed') = (p', changed || changed')

-- | Prune possible values for a particular position.
--
-- Also returns a boolean value signaling whether anything has been pruned.
prunePosition :: Board -> Pending -> (Int,Int) -> Set Int -> (Pending, Bool)
prunePosition b p e candidates
  | candidates == candidates' = (p, False)
  | otherwise                 = (p', True)
  where candidates' = Set.difference candidates $ conflicting b p e
        p' = Map.insert e candidates' p

-- | Collect values that exist in the same row, column, or square.
conflicting :: Board -> Pending -> (Int,Int) -> Set Int
conflicting b p e = Set.unions $ map (\f -> f b p e) [definiteInRow, definiteInColumn, definiteInSquare]

-- | Definite values in the same row.
definiteInRow :: Board -> Pending -> (Int,Int) -> Set Int
definiteInRow b p e = getDefinite b p $ rowPositions Map.! e

-- | Definite values in the same column.
definiteInColumn :: Board -> Pending -> (Int,Int) -> Set Int
definiteInColumn b p e = getDefinite b p $ columnPositions Map.! e

-- | Definite values in the same square.
definiteInSquare :: Board -> Pending -> (Int,Int) -> Set Int
definiteInSquare b p e = getDefinite b p $ squarePositions Map.! e

-- | Collects the definite values from the given positions.
getDefinite :: Board -> Pending -> [(Int,Int)] -> Set Int
getDefinite b p l = Set.fromList $ mapMaybe (lookupDefinite b p) l

-- | Returns a definite value from a position.
-- If the position does not have a definite value, returns Nothing.
lookupDefinite :: Board -> Pending -> (Int,Int) -> Maybe Int
lookupDefinite b p e = onBoard (b Array.! e)
  where onBoard Nothing  = onPending $ Map.lookup e p
        onBoard (Just n) = Just n
        onPending Nothing = Nothing
        onPending (Just s)
          | Set.size s == 1 = Just $ Set.findMin s
          | otherwise       = Nothing

-- | For each position, maps the other positions which reside in the same row.
rowPositions :: Map (Int,Int) [(Int,Int)]
rowPositions =  Map.fromList $ map (\pos -> (pos, row pos)) boardLocations
  where row (x,y) = map (x,) $ List.delete y [1..9]

-- | For each position, maps the other positions which reside in the same column.
columnPositions :: Map (Int,Int) [(Int,Int)]
columnPositions = Map.fromList $ map (\pos -> (pos, column pos)) boardLocations
  where column (x,y) = map (,y) $ List.delete x [1..9]

-- | For each position, maps the other positions which reside in the same square.
squarePositions :: Map (Int,Int) [(Int,Int)]
squarePositions = Map.fromList $ map (\pos -> (pos, xys pos)) boardLocations
  where xys (x,y) = [(x',y') | x' <- bucket x, y' <- bucket y, (x',y') /= (x,y)]
        bucket z = [3 * ((z-1) `div` 3) + 1 .. 3 * ((z-1) `div` 3) + 3]

boardLocations :: [(Int,Int)]
boardLocations = [(x,y) | x <- [1..9], y <- [1..9]]

-- | When pruning no longer works, try guessing through possibilities at a random position.
guess :: RandomGen g => Board -> Pending -> g -> (Maybe Board, g)
guess b p g
  | ((candidate, _) : _) <- ranked = guessWithPosition b' p' candidate g''
  | otherwise = (Nothing, g'')
  where
    -- Incorporate possibilities that have become definite into the board itself.
    (b', p') = incorporatePending b p
    -- We will pick a position with the smallest number of possibilities to constrain the search space more.
    ranked = sortOn snd candidates
    -- Associate each position with a random number as well.  It will serve as a random tiebreaker.
    candidates = zipWith (curry (\((e, s), r) -> ((e, s), (Set.size s, r)))) (Map.toList p') (randoms g' :: [Int])
    (g', g'') = split g

-- | Guess the definite value for a particular position which will result in a solution.
guessWithPosition :: RandomGen g => Board -> Pending -> ((Int,Int), Set Int) -> g -> (Maybe Board, g)
guessWithPosition b p (e,s) g
  | Set.null s = (Nothing, g)
  | otherwise  = continue $ solve b p' g'
  where (i, g') = randomR (0, Set.size s - 1) g
        n = Set.elemAt i s
        s' = Set.deleteAt i s
        p' = Map.insert e (Set.singleton n) p
        continue (Nothing, gen)  = guessWithPosition b p (e, s') gen
        continue (solution, gen) = (solution, gen)
