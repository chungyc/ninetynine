{- |
Description: Crossword puzzles
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P99".
-}
module Problems.P99 (solveCrossword, crosswordPuzzle, crosswordPuzzle') where

import           Problems.Crosswords
import qualified Solutions.P99       as Solution

{- |
Given an empty, or almost empty, crossword puzzle grid and a set of words,
the problem is to place the words on the grid.

Words are strings of at least two characters.
A horizontal or vertical sequence of spots in the crossword puzzle grid is called a site.
Our problem is to find a compatible way of placing words onto sites.
Each word should be placed at most once at a site.

Try to solve the \(25 \times 25\) crossword puzzle in 'crosswordPuzzle''.

=== Examples

>>> :{
crosswordPuzzle ==  Crossword
  { word = ["ALPHA", "ARES", "POPPY"]
  , grid = [ [ Left False, Left False, Left True,  Left False, Left False ]
           , [ Left False, Left False, Left True,  Left False, Left False ]
           , [ Left True,  Left True,  Left True,  Left True,  Left True  ]
           , [ Left False, Left False, Left True,  Left False, Left True  ]
           , [ Left False, Left False, Left True,  Left False, Left True  ]
           , [ Left False, Left False, Left False, Left False, Left True  ]
           ]
  }
:}
True

>>> head $ (\(Just p) -> p) $ solveCrossword crosswordPuzzle
[Nothing,Nothing,Just 'P',Nothing,Nothing]

>>> printCrossword $ solveCrossword crosswordPuzzle
■ ■ P ■ ■
■ ■ O ■ ■
A L P H A
■ ■ P ■ R
■ ■ Y ■ E
■ ■ ■ ■ S
-}
solveCrossword :: Crossword -> Maybe [[Maybe Char]]
solveCrossword = Solution.solveCrossword

-- | A crossword puzzle of size \(5 \times 6\).
--
-- This is the puzzle used as the example for 'solveCrossword'.
crosswordPuzzle :: Crossword
crosswordPuzzle = Crossword
  { word = ["ALPHA", "ARES", "POPPY"]
  , grid = [ [ Left False, Left False, Left True,  Left False, Left False ]
           , [ Left False, Left False, Left True,  Left False, Left False ]
           , [ Left True,  Left True,  Left True,  Left True,  Left True  ]
           , [ Left False, Left False, Left True,  Left False, Left True  ]
           , [ Left False, Left False, Left True,  Left False, Left True  ]
           , [ Left False, Left False, Left False, Left False, Left True  ]
           ]
 }

-- | A crossword puzzle of size \(25 \times 25\).
--
-- This is the puzzle in file @p99d.dat@ included with problem 99 in the original list of problems.
crosswordPuzzle' :: Crossword
crosswordPuzzle' = Crossword
  { word = [ "BANI"
           , "HAUS"
           , "NETZ"
           , "LENA"
           , "ANKER"
           , "ARIEL"
           , "GASSE"
           , "INNEN"
           , "ORADE"
           , "SESAM"
           , "SIGEL"
           , "ANGOLA"
           , "AZETAT"
           , "EKARTE"
           , "NATTER"
           , "NENNER"
           , "NESSEL"
           , "RITTER"
           , "SOMMER"
           , "TAUNUS"
           , "TRANIG"
           , "AGENTUR"
           , "ERRATEN"
           , "ERREGER"
           , "GELEISE"
           , "HAENDEL"
           , "KAROSSE"
           , "MANAGER"
           , "OSTEREI"
           , "SIDERIT"
           , "TERRIER"
           , "ANATOMIE"
           , "ANPASSEN"
           , "BARKASSE"
           , "BEDANKEN"
           , "DEKADENT"
           , "EINLADEN"
           , "ERLASSEN"
           , "FRAGMENT"
           , "GARANTIE"
           , "KRAWATTE"
           , "MEISTERN"
           , "REAKTION"
           , "TENTAKEL"
           , "TRIANGEL"
           , "UEBERALL"
           , "VERGEBEN"
           , "AFRIKANER"
           , "BESTELLEN"
           , "BULLAUGEN"
           , "SANTANDER"
           , "VERBERGEN"
           , "ALLENSTEIN"
           , "AUSTRALIEN"
           , "BETEILIGEN"
           , "NATALITAET"
           , "OBERHAUSEN"
           , "UNTERSTAND"
           , "LEUMUND"
           ]
  , grid = [ [t,t,t,t,t,t,t,t,f,t,t,t,t,t,t,t,t,f,t,t,t,t,t,t,t]
           , [t,f,f,f,t,f,f,f,f,t,f,f,f,t,f,f,f,f,t,f,t,f,f,f,t]
           , [t,f,t,f,t,f,t,t,t,t,t,t,t,t,t,t,f,f,t,f,t,f,f,f,t]
           , [t,t,t,t,t,t,t,f,f,t,f,f,f,t,f,t,f,t,t,t,t,t,t,t,t]
           , [t,f,t,f,t,f,t,f,f,t,f,t,f,t,f,t,f,f,t,f,t,f,t,f,t]
           , [t,f,t,f,t,f,t,f,t,t,t,t,t,t,f,t,f,f,f,f,t,f,t,f,t]
           , [t,f,t,f,t,f,t,f,f,f,f,t,f,t,t,t,t,t,t,t,t,f,t,f,f]
           , [t,f,t,f,t,t,t,t,t,t,f,t,f,t,f,t,f,t,f,f,t,f,t,f,t]
           , [t,f,t,f,f,t,f,f,t,f,f,t,f,f,f,t,f,t,f,f,f,f,t,f,t]
           , [t,t,t,t,t,t,f,f,t,t,t,t,t,t,f,t,f,t,f,t,t,t,t,t,t]
           , [f,f,f,f,f,t,f,f,t,f,f,t,f,t,f,t,f,t,f,t,f,f,t,f,t]
           , [t,t,t,t,t,t,t,f,t,f,f,t,f,t,f,t,t,t,t,t,t,t,f,f,t]
           , [t,f,f,f,f,t,f,f,t,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,t]
           , [t,f,t,f,f,t,t,t,t,t,t,t,f,t,t,t,t,t,t,t,t,f,f,f,t]
           , [t,f,t,f,f,f,f,f,t,f,f,f,f,t,f,f,f,f,t,f,f,f,f,f,t]
           , [t,t,t,t,t,t,f,t,f,t,t,t,t,t,t,t,f,t,t,t,t,t,t,t,t]
           , [t,f,t,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,t,f,t,f,f,f,t]
           , [t,f,t,f,t,t,t,t,t,t,t,t,t,f,f,f,t,f,t,f,t,f,f,f,f]
           , [t,f,t,f,f,f,f,t,f,t,f,f,t,f,f,f,t,f,t,f,t,t,t,t,t]
           , [f,f,t,f,f,f,f,t,f,f,t,t,t,t,t,t,t,f,t,f,t,f,f,f,t]
           , [t,t,t,t,t,t,t,t,t,t,f,f,t,f,f,f,t,f,f,f,f,t,f,f,t]
           , [t,f,t,f,f,f,f,t,f,f,t,f,f,t,t,t,t,t,t,t,t,t,f,f,t]
           , [t,f,f,t,t,t,t,t,t,t,t,t,f,t,f,f,t,f,f,f,f,t,f,f,t]
           , [t,f,f,f,f,f,f,t,f,f,t,f,f,t,f,f,t,f,f,f,f,t,f,f,t]
           , [t,t,t,t,t,t,t,t,f,f,t,t,t,t,t,t,t,t,t,f,t,t,t,t,t]
           ]
  }
  where f = Left False
        t = Left True
