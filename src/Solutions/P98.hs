{- |
Description: Nonograms
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P98" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P98 (nonogram) where

import           Control.Monad.State.Lazy
import           Data.Array               (Array, listArray, (!), (//))
import qualified Data.Array               as Array
import           Data.Foldable            (foldlM)
import           Data.List                (group, sortOn)
import           Data.Maybe               (fromJust, isJust, isNothing)
import           System.Random

-- | Solve a nonogram.
nonogram :: [[Int]]  -- ^ Lengths of occupied cells in each row
         -> [[Int]]  -- ^ Lengths of occupied cells in each column
         -> Maybe [[Bool]]  -- ^ Solution to the puzzle, if it exists
nonogram rows columns = fst $ randomNonogram rows columns $ mkStdGen 11111

-- | A nonogram solver, which uses the given source of randomness if it needs to make any guesses.
randomNonogram :: RandomGen g
               => [[Int]]  -- ^ Lengths of occupied cells in each row
               -> [[Int]]  -- ^ Lengths of occupied cells in each column
               -> g        -- ^ Random generator
               -> (Maybe [[Bool]], g)  -- ^ Solution to the puzzle, if it exists, and random generator
randomNonogram rows columns gen = (toList solution, gen')
  where (solution, gen') = fillBitmap rows columns [1..length rows] [1..length columns] blank gen
        blank = blankBitmap (length rows) (length columns)

-- | Stores the partially built bitmap.
-- Positions with 'Just' values are those that definitely have a certain bit.
type Bitmap = Array (Int,Int) (Maybe Bool)

-- | Stores a partially built line.
-- May be a row or a column in the partially built bitmap.
type Line = Array Int (Maybe Bool)

blankBitmap :: Int -> Int -> Bitmap
blankBitmap rows columns = listArray ((1,1), (rows,columns)) $ repeat Nothing

toList :: Maybe Bitmap -> Maybe [[Bool]]
toList Nothing = Nothing
toList (Just picture) = Just $ map (\r -> map (\c -> fromJust $ picture ! (r,c)) [1..columns]) [1..rows]
  where rows = getRowSize picture
        columns = getColumnSize picture

getRowSize :: Bitmap -> Int
getRowSize picture = fst $ snd $ Array.bounds picture

getColumnSize :: Bitmap -> Int
getColumnSize picture = snd $ snd $ Array.bounds picture

-- | The high level body for solving a nonogram.
-- It basically goes as:
--
-- 1. Tries to determine as many definite bits as it can.
--
-- 2. If all bits are definite, we have a solution.
--    If a contradictory bit appears, there is no solution.
--
-- 3. Repeat the search for definite bits until no more definite bits are found.
--    If there are still indefinite bits, pretend that one of them is definite and go back to 1.
fillBitmap :: RandomGen g => [[Int]] -> [[Int]] -> [Int] -> [Int] -> Bitmap -> g -> (Maybe Bitmap, g)
fillBitmap rows columns remainingRows remainingColumns picture gen
  | all isJust picture      = case isConsistentWithPuzzle picture rows columns of
                                True  -> (Just picture, gen)
                                False -> (Nothing, gen)
  | isNothing maybePicture' = (Nothing, gen)
  | picture == picture'     = guess rows columns remainingRows remainingColumns picture gen
  | otherwise               = fillBitmap rows columns remainingRows' remainingColumns' picture' gen
  where maybePicture' = do
          p <- fillRows rows remainingRows picture
          fillColumns columns remainingColumns p
        picture' = fromJust maybePicture'
        remainingRows' = filter isIncompleteRow remainingRows
        remainingColumns' = filter isIncompleteColumn remainingColumns
        isIncompleteRow r = any (\c -> isNothing $ picture' ! (r,c)) [1..getColumnSize picture']
        isIncompleteColumn c = any (\r -> isNothing $ picture' ! (r,c)) [1..getRowSize picture']

isConsistentWithPuzzle :: Bitmap -> [[Int]] -> [[Int]] -> Bool
isConsistentWithPuzzle picture rows columns = withRows && withColumns
  where withRows = rows == map (lengths . getRow picture) [1..getRowSize picture]
        withColumns = columns == map (lengths . getColumn picture) [1..getColumnSize picture]
        lengths = map length . filter occupied . group . map fromJust . Array.elems
        occupied (v:_) = v
        occupied [] = False

fillRows :: [[Int]] -> [Int] -> Bitmap -> Maybe Bitmap
fillRows rows remainingRows p = foldlM (\p' i -> fill p' i $ rows !! (i-1)) p remainingRows
  where fill picture row lengths = replaceRow picture row $ fillLine lengths $ getRow picture row

fillColumns :: [[Int]] -> [Int] -> Bitmap -> Maybe Bitmap
fillColumns columns remainingColumns p = foldlM (\p' i -> fill p' i $ columns !! (i-1)) p remainingColumns
  where fill picture column lengths = replaceColumn picture column $ fillLine lengths $ getColumn picture column

getRow :: Bitmap -> Int -> Line
getRow picture row = listArray (1,length cells) cells
  where cells = [picture ! (row,column) | column <- [1..getColumnSize picture]]

getColumn :: Bitmap -> Int -> Line
getColumn picture column = listArray (1,length cells) cells
  where cells = [picture ! (row,column) | row <- [1..getRowSize picture]]

replaceRow :: Bitmap -> Int -> Maybe Line -> Maybe Bitmap
replaceRow _ _ Nothing = Nothing
replaceRow picture row (Just line) = Just $ picture // cells
  where cells = [((row,column), line ! column) | column <- [1..getColumnSize picture]]

replaceColumn :: Bitmap -> Int -> Maybe Line -> Maybe Bitmap
replaceColumn _ _ Nothing = Nothing
replaceColumn picture column (Just line) = Just $ picture // cells
  where cells = [((row,column), line ! row) | row <- [1..getRowSize picture]]

-- | When there are no more bits that can be inferred to be definite,
-- pick a bit at random and see what happens if we pretend it's definite.
guess :: RandomGen g => [[Int]] -> [[Int]] -> [Int] -> [Int] -> Bitmap -> g -> (Maybe Bitmap, g)
guess rows columns remainingRows remainingColumns picture =
  runState (guess' rows columns remainingRows remainingColumns picture)

guess' :: RandomGen g => [[Int]] -> [[Int]] -> [Int] -> [Int] -> Bitmap -> State g (Maybe Bitmap)
guess' rows columns remainingRows remainingColumns picture = do
  tags <- rnds  -- Random numbers used for random tie breaking during sorting.
  value <- rnd  -- Definite value to try first
  let ranked = sortOn (countIndefiniteNeighbors picture) $ zip candidates tags
  case ranked of
    [] -> return Nothing
    ((candidate, _) : _) -> do
      picture' <- fill $ picture // [(candidate, Just value)]
      case picture' of
        Nothing -> fill $ picture // [(candidate, Just $ not value)]
        _       -> return picture'
  where candidates = filter (\p -> isNothing $ picture ! p) [(r,c) | r <- remainingRows, c <- remainingColumns]
        fill p = state $ fillBitmap rows columns remainingRows remainingColumns p

-- | For sorting positions so that those that have more definite values in the same row and column come first.
-- This will hopefully make it more likely to cause a contradiction earlier if there is no solution
-- when we set a definite value for a position, i.e., prunes the search space much more.
countIndefiniteNeighbors :: Bitmap -> ((Int,Int),Int) -> (Int,Int)
countIndefiniteNeighbors picture ((row,column),rx) = (rowCount + columnCount,rx)
  where rowCount = sum [toInt $ picture ! (row, c) | c <- [1..getColumnSize picture]]
        columnCount = sum [toInt $ picture ! (r, column) | r <- [1..getRowSize picture]]
        toInt Nothing = 1
        toInt _       = 0

rnd :: (RandomGen g, Random a) => State g a
rnd = state random

rnds :: (RandomGen g, Random a) => State g [a]
rnds = do gen <- state split
          return $ randoms gen

-- The definitions above deals with the scaffolding for solving the problem.
-- What comes below is the heart of the logic for inferring bits in the bitmap.

-- | Fill the line with more definite bits, if any.
fillLine :: [Int] -> Line -> Maybe Line
fillLine xs line
  | isConsistent xs line'    = Just $ toArray line'
  | null possibleLines       = Nothing
  | isDone && isInconsistent = Nothing
  | otherwise                = Just $ toArray incorporated
  where
    bits = Array.elems line
    toArray l = listArray (1,length l) l
    line' = map (\x -> case x of Nothing -> Just False; _ -> x) bits

    -- From bits that are the same for all possible lines,
    -- infer that they are definite.
    definiteBits = foldl1 merge possibleLines
    possibleLines = filter (isConsistent xs) $ fillLine' 1 xs bits
    merge = zipWith combine
    combine (Just u) (Just v) | u == v    = Just u
                              | otherwise = Nothing
    combine _ _ = Nothing

    -- For incorporating definite bits into the line.
    incorporate = zipWith set
    set Nothing v  = v
    set u Nothing  = u
    set _ (Just u) = Just u

    incorporated = incorporate bits definiteBits
    isDone = all isJust incorporated
    isInconsistent = not $ isConsistent xs incorporated

isConsistent :: [Int] -> [Maybe Bool] -> Bool
isConsistent [] line = all (Just False ==) line
isConsistent xs line = xs == xs'
  where xs' = map length $ filter occupied $ group line
        occupied (Just v : _) = v
        occupied _ = False

fillLine' :: Int -> [Int] -> [Maybe Bool] -> [[Maybe Bool]]
fillLine' _ [] line         = [line]
fillLine' start (x:xs) line = concatMap (fillSegment line xs x) positions
  where positions = [start..length line - x + 1]

-- | With the given length and position,
-- place a segment with consecutive 'True's followed by a 'False'.
-- If this would result in a contradiction, 'Nothing' is returned.
fillSegment :: [Maybe Bool] -> [Int] -> Int -> Int -> [[Maybe Bool]]
fillSegment line xs len pos
  | pos+len-1 > length line = []
  | any isNothing line'     = []
  | otherwise               = fillLine' (pos+len+1) xs $ map fromJust line'
  where segment = replicate (pos-1) Nothing ++ replicate len (Just True) ++ [Just False] ++ repeat Nothing
        line' = zipWith combineCell line segment

combineCell :: Maybe Bool -> Maybe Bool -> Maybe (Maybe Bool)
combineCell Nothing Nothing = Just Nothing
combineCell b Nothing = Just b
combineCell Nothing b = Just b
combineCell (Just a) (Just b) | a == b    = Just $ Just a
                              | otherwise = Nothing
