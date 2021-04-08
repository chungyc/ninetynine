{- |
Description: Crossword puzzles
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P99" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P99 (solveCrossword, randomSolveCrossword) where

import           Control.Monad
import           Control.Monad.State.Lazy
import           Data.List                (delete, sortOn, transpose)
import           Data.Map.Lazy            (Map, (!))
import qualified Data.Map.Lazy            as Map
import           Data.Maybe               (isNothing)
import           Data.Tuple               (swap)
import           Problems.Crosswords
import           Problems.P25
import           System.Random

{- |
Given an empty, or almost empty, crossword puzzle grid and a set of words,
the problem is to place the words into the framework.

Words are strings of at least two characters.
A horizontal or vertical sequence of spots in the crossword puzzle grid is called a site.
Our problem is to find a compatible way of placing words onto sites.
Each word should be placed at most once at a site.
-}
solveCrossword :: Crossword -> Maybe [[Maybe Char]]
solveCrossword p = fst $ randomSolveCrossword p $ mkStdGen 99

-- | Solves a crossword puzzle.
--
-- When there is a need to make a guess, the given source of randomness is used.
randomSolveCrossword :: RandomGen g => Crossword -> g -> (Maybe [[Maybe Char]], g)
randomSolveCrossword p gen = (fromPartial solution, gen')
  where (solution, gen') = build (toPartial p) gen

-- | Partial solution being built up, and associated data supporting the buildup.
data Partial = Partial
  { sizes        :: (Int,Int)
  , sites        :: Map Int Site          -- ^ Indexed sites
  , candidates   :: Map Int [String]      -- ^ Possible words for each site.
  , partialWords :: Map Int [Maybe Char]  -- ^ Partially constructed words for sites
  , fullWords    :: Map Int String        -- ^ Fully constructed words for sites
  }
  deriving Show

-- | Structure of a site in a crossword puzzle to be filled with a word.
data Site = Site { size        :: Int
                 , position    :: (Int,Int)
                 , orientation :: Orientation
                 , crossovers  :: [CrossoverPoint]
                 }
  deriving Show

-- | Orientation of a site.
data Orientation = Horizontal | Vertical
  deriving (Eq, Show)

-- | Information about a crossover point between sites.
--
-- Holds position inside site, position inside the other site, and index of other site.
data CrossoverPoint = CrossoverPoint Int Int Int
  deriving Show

-- Functions related to turning the puzzle and solution to and from internal and external forms.

toPartial :: Crossword -> Partial
toPartial (Crossword { word = ws, grid = g }) =
  Partial { sizes = (length g, maximum $ map length g)
          , sites = ss
          , candidates = Map.map (\s -> Map.findWithDefault [] (size s) sizedWords) ss
          , partialWords = pws
          , fullWords = Map.empty
          }
  where (ss, pws) = toSites g
        sizedWords = Map.fromListWith (++) $ map (\w -> (length w, [w])) ws

toSites :: [[Either Bool Char]] -> (Map Int Site, Map Int [Maybe Char])
toSites g = (markCrossovers indexedSites, indexedWords)
  where sitesList = findSites g Horizontal ++ transposeSites (findSites (transpose g) Vertical)
        indexedSites = Map.fromList $ zip [1..] $ map fst sitesList
        indexedWords = Map.fromList $ zip [1..] $ map snd sitesList
        transposeSites = map transposePos
        transposePos (s@(Site { position = (row,col) }), pw) = (s { position = (col,row) }, pw)

findSites :: [[Either Bool Char]] -> Orientation -> [(Site, [Maybe Char])]
findSites g orient = concat $ map (\l -> evalState find $ initial l) $ zip [0..] g
  where
    find = do
      s <- gets fssSpots
      ls <- gets fssLocs
      case s of
        []               -> return ls
        (Left False : _) -> step >> find
        _                -> startLocation

    startLocation = do
      modify $ \s -> s { fssStart = fssPos s
                       , fssSize = 0
                       , fssChars = []
                       }
      extract

    extract = do
      s <- gets fssSpots
      case s of
        Right c : _   -> addSpot $ Just c
        Left True : _ -> addSpot Nothing
        _             -> endLocation

    addSpot s =  do
      modify $ \st -> st { fssSize = 1 + fssSize st
                         , fssChars = s : fssChars st
                         }
      step
      extract

    endLocation = do
      st <- get
      let s = Site { size = fssSize st
                   , position = (fssRow st, fssStart st)
                   , orientation = orient
                   , crossovers = []
                   }
      when (fssSize st > 1) $
        put st { fssLocs = (s, reverse $ fssChars st) : fssLocs st }
      find

    initial (row, line) = FindSitesState { fssSpots = line
                                         , fssRow = row
                                         , fssPos = 0
                                         , fssStart = 0
                                         , fssSize = 0
                                         , fssChars = []
                                         , fssLocs = []
                                         }
    step = modify $ \st -> st { fssSpots = tail $ fssSpots st
                              , fssPos = 1 + fssPos st
                              }

-- | Monadic state for 'findSites'.
data FindSitesState = FindSitesState
  { fssSpots :: [Either Bool Char]
  , fssRow   :: Int
  , fssPos   :: Int
  , fssStart :: Int
  , fssSize  :: Int
  , fssChars :: [Maybe Char]
  , fssLocs  :: [(Site, [Maybe Char])]
  }

markCrossovers :: Map Int Site -> Map Int Site
markCrossovers sitesMap = Map.foldlWithKey mark sitesMap crossoverSpots
  where taggedSpots = tagSpots sitesMap
        crossoverSpots = Map.filter ((<) 1 . length) taggedSpots
        mark sitesMap' pos siteIndexes = Map.unionWith mergeSite (markedSites sitesMap' pos siteIndexes) sitesMap'
        mergeSite s@(Site { crossovers = c }) (Site { crossovers = c' }) = s { crossovers = c ++ c' }
        markedSites m p is = Map.fromList $ map (markSite m p is) is
        markSite m pos indexes index =
          let site = m ! index
              indexes' = delete index indexes
              cs = map (getCrossover m pos site) indexes'
          in (index, site { crossovers = cs })
        getCrossover m (row,column) site index =
          let otherSite = m ! index
              offset (Site {position = (_,c), orientation = Horizontal}) = column - c
              offset (Site {position = (r,_), orientation = Vertical})   = row - r
          in CrossoverPoint (offset site) (offset otherSite) index

tagSpots :: Map Int Site -> Map (Int,Int) [Int]
tagSpots sitesMap = Map.foldlWithKey tag Map.empty sitesMap
  where tag taggedSpots index site = Map.unionWith (++) taggedSpots $ siteSpots index site
        siteSpots index site = Map.fromList $ zip (positions site) $ repeat [index]
        positions Site { size = n, position = pos, orientation = o } = getPositions n pos o
        getPositions n (row,column) Horizontal = [(row, column+i) | i <- [0..n-1]]
        getPositions n (row,column) Vertical   = [(row+i, column) | i <- [0..n-1]]

fromPartial :: Maybe Partial -> Maybe [[Maybe Char]]
fromPartial Nothing  = Nothing
fromPartial (Just s) = Just $ g'
  where (rowCount, columnCount) = sizes s
        blank = replicate rowCount $ replicate columnCount Nothing
        wordSites = zip (Map.elems $ sites s) (Map.elems $ fullWords s)
        horizWordSites = filter ((==) Horizontal . orientation . fst) wordSites
        vertWordSites = filter ((==) Vertical . orientation . fst) wordSites
        g  = foldl incorporateWord blank horizWordSites
        g' = transpose $ foldl incorporateWord (transpose g) vertWordSites

incorporateWord :: [[Maybe Char]] -> (Site, String) -> [[Maybe Char]]
incorporateWord g (s, w) = take r g ++ [row'] ++ drop (r+1) g
  where (r,c) = case orientation s of
          Horizontal -> position s
          Vertical   -> swap $ position s
        row = g !! r
        row' = take c row ++ map Just w ++ drop (c + length w) row

-- Functions that are responsible for actually solving a crossword puzzle.

{- |
High-level driving function for solving the crossword puzzle.

1. Determine as many letters and word placements that are definite.

2. If all blank spots are filled, we have a solution.
   If we are in a state where a contradiction is inevitable,
   there is no solution.

3. If there are no more letters or words that can be placed definitely,
   guess a word placement and go back to 1.
-}
build :: RandomGen g => Partial -> g -> (Maybe Partial, g)
build partial gen
  | Map.null $ pws         = (Just partial, gen)
  | isNothing maybePartial = (Nothing, gen)
  | isUnchanged            = guess partial gen
  | otherwise              = build partial' gen
  where cs = candidates partial
        pws = partialWords partial
        fws = fullWords partial
        maybePartial = infer partial
        Just partial' = maybePartial
        isUnchanged = (cs, pws, fws) == (candidates partial', partialWords partial', fullWords partial')

-- | Infer further placements of letters and words.
infer :: Partial -> Maybe Partial
infer partial = foldM pruneSiteCandidateWords partial $ Map.keys $ candidates partial

pruneSiteCandidateWords :: Partial -> Int -> Maybe Partial
pruneSiteCandidateWords partial index = case cs' of
  []  -> Nothing
  [c] -> Just $ affixWord partial index c
  _   -> Just $ partial { candidates = Map.insert index cs' $ candidates partial }
  where cs' = filter (isConsistentWord pw) cs
        cs = candidates partial ! index
        pw = partialWords partial ! index

isConsistentWord :: [Maybe Char] -> String -> Bool
isConsistentWord [] []                   = True
isConsistentWord (Nothing : xs) (_ : ys) = isConsistentWord xs ys
isConsistentWord (Just x : xs) (y : ys)  = x == y && isConsistentWord xs ys
isConsistentWord _ _                     = False

affixWord :: Partial -> Int -> String -> Partial
affixWord p i w = foldl fill p' $ crossovers $ sites p' ! i
  where p' = p { candidates = Map.delete i $ remove $ candidates p
               , partialWords = Map.delete i $ partialWords p
               , fullWords = Map.insert i w $ fullWords p
               }
        fill q (CrossoverPoint pos pos' i') =
          case Map.lookup i' $ partialWords q of
            Nothing -> q
            Just w' -> let w'' = take pos' w' ++ [Just $ w !! pos] ++ drop (pos'+1) w'
                       in q { partialWords = Map.insert i' w'' $ partialWords q }
        remove ws = Map.map (delete w) ws

-- | If no further sites can be definitely filled,
-- pick a site and guess a word to fill it with,
-- and continue to definitely fill the puzzle.
guess :: RandomGen g => Partial -> g -> (Maybe Partial, g)
guess p gen = runState (guess' p) gen

guess' :: RandomGen g => Partial -> State g (Maybe Partial)
guess' p = do
  tags <- rnds  -- used for random tie breaking with sorting
  let siteIndex = snd $ head $ sortOn count $ zip (tags :: [Int]) $ Map.keys $ candidates p
  wordList <- state $ randomPermute $ candidates p ! siteIndex
  state $ tryGuesses p siteIndex wordList
  where count (t, i) = (length $ candidates p ! i, t)

rnds :: (RandomGen g, Random a) => State g [a]
rnds = do
  (gen, gen') <- gets split
  put gen'
  return $ randoms gen

tryGuesses :: RandomGen g => Partial -> Int -> [String] -> g -> (Maybe Partial, g)
tryGuesses _ _ [] gen = (Nothing, gen)
tryGuesses p i (w:ws) gen =
  case build (affixWord p i w) gen of
    (Nothing, gen') -> tryGuesses p i ws gen'
    solution        -> solution
