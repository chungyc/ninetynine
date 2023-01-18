{- |
Description: Crossword puzzles
Copyright: Copyright (C) 2023 Yoo Chung
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
import           Data.Maybe               (fromJust, isNothing)
import           Data.Tuple               (swap)
import           Problems.Crosswords
import           Problems.P25
import           System.Random

-- | Solve a crossword puzzle.
solveCrossword :: Crossword -> Maybe [[Maybe Char]]
solveCrossword p = fst $ randomSolveCrossword p $ mkStdGen 99

-- | Solves a crossword puzzle.
--
-- When there is a need to make a guess, the given source of randomness is used.
randomSolveCrossword :: RandomGen g => Crossword -> g -> (Maybe [[Maybe Char]], g)
randomSolveCrossword p gen = (fromPartial solution, gen')
  where (solution, gen') = build (toPartial p) gen

{- |
Partial solution being built up, and associated data supporting the buildup.

Instead of trying fill the grid directly,
the crossword puzzle is turned into a graph of sites,
where each site is associated with the list of possible words,
partially filled in characters, and definite words if found.
The edges are formed by the crossover points between sites,
and are labeled by how they cross over.

The solver will try to prune candidate words from sites,
or guess what the word for a site in a solution may be.
When a site is filled with a word, it updates its neighbors in the graph
to fill additional characters in crossing sites.

Maps are indexed by numbers which identify individual sites.
-}
data Partial = Partial
  { sizes        :: (Int,Int)             -- ^ Number of rows and columns
  , sites        :: Map Int Site          -- ^ Sites, indexed by numbers identifiying sites

  -- The above is a static description of the puzzle.
  -- The below will be updated as the solution is built up.
  -- 'candidates' and 'partialWords' have the same keys,
  -- while 'fullWords' is keyed by their complement.

  , candidates   :: Map Int [String]      -- ^ Possible words for each site
  , partialWords :: Map Int [Maybe Char]  -- ^ Partially constructed words for sites
  , fullWords    :: Map Int String        -- ^ Fully constructed words for sites
  }

-- | Structure of a site in a crossword puzzle to be filled with a word.
data Site = Site
  { size        :: Int               -- ^ Lenth of word that should fill the site
  , position    :: (Int,Int)         -- ^ Position in grid in (row,column)
  , orientation :: Orientation       -- ^ Whether site is horizontal or vertical
  , crossovers  :: [CrossoverPoint]  -- ^ How the site crosses over with other sites
  }

-- | Orientation of a site.
data Orientation = Horizontal | Vertical deriving Eq

-- | Information about a crossover point between sites.
--
-- Holds position inside site, position inside the other site, and index of other site.
data CrossoverPoint = CrossoverPoint Int Int Int

-- Functions related to turning the puzzle and solution to and from internal and external forms.

-- | Translate the crossword puzzle into a form used internally to solve the puzzle.
toPartial :: Crossword -> Partial
toPartial Crossword{ word = ws, grid = g } =
  Partial { sizes = (length g, maximum $ map length g)
          , sites = ss
          , candidates = Map.map (\s -> Map.findWithDefault [] (size s) sizedWords) ss
          , partialWords = pws
          , fullWords = Map.empty
          }
  where (ss, pws) = toSites g
        sizedWords = Map.fromListWith (++) $ map (\w -> (length w, [w])) ws

-- | Convert a grid to sites and possible characters.
-- The latter will be 'Nothing' unless there is a prefilled character in a spot.
toSites :: [[Either Bool Char]] -> (Map Int Site, Map Int [Maybe Char])
toSites g = (markCrossovers indexedSites, indexedWords)
  where sitesList = findSites g Horizontal ++ findSites (transpose g) Vertical
        indexedSites = Map.fromList $ zip [1..] $ map fst sitesList
        indexedWords = Map.fromList $ zip [1..] $ map snd sitesList

-- | From the rows in the grid, find the sites and their prefilled characters.
findSites :: [[Either Bool Char]] -> Orientation -> [(Site, [Maybe Char])]
findSites g orient = concatMap (evalState find . initial) $ zip [0..] g
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
                   , position = case orient of
                       Horizontal -> (fssRow st, fssStart st)
                       Vertical   -> (fssStart st, fssRow st)
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

-- | Monadic state for findSites.
data FindSitesState = FindSitesState
  { fssSpots :: [Either Bool Char]
  , fssRow   :: Int
  , fssPos   :: Int
  , fssStart :: Int
  , fssSize  :: Int
  , fssChars :: [Maybe Char]
  , fssLocs  :: [(Site, [Maybe Char])]
  }

-- | Mark the crossover points in each site.
markCrossovers :: Map Int Site -> Map Int Site
markCrossovers sitesMap = Map.foldlWithKey mark sitesMap crossoverSpots
  where taggedSpots = tagSpots sitesMap
        crossoverSpots = Map.filter ((<) 1 . length) taggedSpots
        mark sitesMap' pos siteIndexes = Map.unionWith mergeSite (markedSites sitesMap' pos siteIndexes) sitesMap'
        mergeSite s@Site{ crossovers = c } Site{ crossovers = c' } = s { crossovers = c ++ c' }
        markedSites m p is = Map.fromList $ map (markSite m p is) is
        markSite m pos indexes index =
          let site = m ! index
              indexes' = delete index indexes
              cs = map (getCrossover m pos site) indexes'
          in (index, site { crossovers = cs })
        getCrossover m (row,column) site index =
          let otherSite = m ! index
              offset Site {position = (_,c), orientation = Horizontal} = column - c
              offset Site {position = (r,_), orientation = Vertical}   = row - r
          in CrossoverPoint (offset site) (offset otherSite) index

-- | Tag each spot in the grid with the sites located on the spot.
tagSpots :: Map Int Site -> Map (Int,Int) [Int]
tagSpots = Map.foldlWithKey tag Map.empty
  where tag taggedSpots index site = Map.unionWith (++) taggedSpots $ siteSpots index site
        siteSpots index site = Map.fromList $ zip (positions site) $ repeat [index]
        positions Site { size = n, position = pos, orientation = o } = getPositions n pos o
        getPositions n (row,column) Horizontal = [(row, column+i) | i <- [0..n-1]]
        getPositions n (row,column) Vertical   = [(row+i, column) | i <- [0..n-1]]

-- | Convert the internal form used for solving the puzzle to the form returned by 'solveCrossword'.
fromPartial :: Maybe Partial -> Maybe [[Maybe Char]]
fromPartial Nothing  = Nothing
fromPartial (Just s) = Just g'
  where (rowCount, columnCount) = sizes s
        blank = replicate rowCount $ replicate columnCount Nothing
        wordSites = zip (Map.elems $ sites s) (Map.elems $ fullWords s)
        horizWordSites = filter ((==) Horizontal . orientation . fst) wordSites
        vertWordSites = filter ((==) Vertical . orientation . fst) wordSites
        g  = foldl incorporateWord blank horizWordSites
        g' = transpose $ foldl incorporateWord (transpose g) vertWordSites

-- | Incorporate the given word at the given site into the crossword grid.
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
  | Map.null pws           = (Just partial, gen)
  | isNothing maybePartial = (Nothing, gen)
  | isUnchanged            = guess partial gen
  | otherwise              = build partial' gen
  where cs = candidates partial
        pws = partialWords partial
        fws = fullWords partial
        maybePartial = infer partial
        partial' = fromJust maybePartial
        isUnchanged = (cs, pws, fws) == (candidates partial', partialWords partial', fullWords partial')

-- | Infer further placements of letters and words.
infer :: Partial -> Maybe Partial
infer partial = foldM pruneSiteCandidateWords partial $ Map.keys $ candidates partial

-- | Prune candidate words that are not possible from the given site.
pruneSiteCandidateWords :: Partial -> Int -> Maybe Partial
pruneSiteCandidateWords partial index = case cs' of
  []  -> Nothing
  [c] -> Just $ affixWord partial index c
  _   -> Just $ partial { candidates = Map.insert index cs' $ candidates partial }
  where cs' = filter (isConsistentWord pw) cs
        cs = candidates partial ! index
        pw = partialWords partial ! index

-- | Whether a word is consistent with the characters that have been determined so far.
isConsistentWord :: [Maybe Char] -> String -> Bool
isConsistentWord [] []                   = True
isConsistentWord (Nothing : xs) (_ : ys) = isConsistentWord xs ys
isConsistentWord (Just x : xs) (y : ys)  = x == y && isConsistentWord xs ys
isConsistentWord _ _                     = False

-- | Affix a word to a site, and all the other changes this entails.
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
        remove = Map.map (delete w)

-- | If no further sites can be definitely filled,
-- pick a site and guess a word to fill it with,
-- and continue to definitely fill the puzzle.
guess :: RandomGen g => Partial -> g -> (Maybe Partial, g)
guess p = runState (guess' p)

guess' :: RandomGen g => Partial -> State g (Maybe Partial)
guess' p = do
  tiebreakers <- do gen <- state split
                    return (randoms gen :: [Int])
  let siteIndex = snd $ head $ sortOn count $ zip tiebreakers $ Map.keys $ candidates p
  wordList <- state $ randomPermute $ candidates p ! siteIndex
  state $ tryGuesses p siteIndex wordList
  where
    count (tag, i) =
      -- Try sites with fewer candidates first.
      ( length $ candidates p ! i
      -- If above equal, try sites with fewer indefinite spots first.
      , length $ filter isNothing $ partialWords p ! i
      -- Random tiebreaker.
      , tag)

-- | Try each guess for a site in the given order.
-- It will return the first solution it can find, if any.
tryGuesses :: RandomGen g => Partial -> Int -> [String] -> g -> (Maybe Partial, g)
tryGuesses _ _ [] gen = (Nothing, gen)
tryGuesses p i (w:ws) gen =
  case build (affixWord p i w) gen of
    (Nothing, gen') -> tryGuesses p i ws gen'
    solution        -> solution
