{-|
Description: Tests for the examples included with the documentation
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Main (main) where

import           Data.List             (isInfixOf)
import           System.Console.GetOpt
import           System.Environment
import           Test.DocTest          (doctest)

main :: IO ()
main = do
  doctest' ["--fast"] stateless
  doctest' [] stateful
  doctest' [] $ mapFiles ["Crosswords.hs"]
  doctest' [] $ mapFiles ["MultiwayTrees.hs"]
  doctest' ["--fast"] solutions

doctest' :: [String] -> [String] -> IO ()
doctest' flags filenames = do
  args <- getArgs
  match <- matchOptions args
  doctest $ flags ++ (match filenames)

data Flag = Match String deriving Show

options :: [OptDescr Flag]
options = [ Option [] ["match"] (ReqArg Match "match") "substring match on file name" ]

-- | Support @--match@ option that is also supported by Hspec.
matchOptions :: [String] -> IO ([String] -> [String])
matchOptions argv =
  case getOpt Permute options argv of
    ([Match m], _, _) -> return $ filter $ isInfixOf m
    _                 -> return $ id

mapFiles :: [String] -> [String]
mapFiles = map ("src/Problems/" ++)

-- | Deterministic examples with no state or local definitions to reset.
stateless :: [String]
stateless = mapFiles
  [ "P01.hs", "P02.hs", "P03.hs", "P04.hs", "P05.hs"
  , "P06.hs", "P07.hs", "P08.hs", "P09.hs", "P10.hs"
  , "P11.hs", "P12.hs", "P13.hs", "P14.hs", "P15.hs"
  , "P16.hs", "P17.hs", "P18.hs", "P19.hs", "P20.hs"
  , "P21.hs", "P22.hs"
  , "P28.hs", "P29.hs", "P30.hs"
  , "P31.hs", "P32.hs", "P33.hs", "P34.hs", "P35.hs"
  , "P36.hs", "P38.hs", "P39.hs", "P40.hs"
  , "P42.hs"
  , "P46.hs", "P47.hs", "P49.hs"
  , "P54.hs", "P55.hs"
  , "P56.hs", "P57.hs", "P59.hs", "P60.hs"
  , "P63.hs", "P64.hs", "P65.hs"
  , "P67.hs", "P70.hs"
  , "P71.hs", "P72.hs", "P73.hs", "P74.hs", "P75.hs"
  , "P76.hs"
  , "P83.hs", "P85.hs"
  , "P89.hs"
  , "P94.hs"
  , "P95.hs", "P96.hs", "P97.hs", "P98.hs", "P99.hs"
  ]

-- | Deterministic examples with local definitions to reset.
stateful :: [String]
stateful = mapFiles [ "P26.hs", "P27.hs", "P48.hs", "P57.hs"
                    , "P61.hs", "P62.hs", "P66.hs", "P68.hs", "P69.hs"
                    , "P81.hs", "P82.hs", "P84.hs", "P87.hs", "P88.hs", "P90.hs"
                    , "P93.hs"
                    ]

-- Examples in solutions.
solutions :: [String]
solutions = map ("src/Solutions/" ++) $ [ "P49.hs" ]

{-
Examples in the following problems are intentionally omitted from testing.

* P23, P24, P25, P51 : Randomness means we cannot expect deterministic output.
  Even with a fixed pseudo-random number generator, we cannot impose any particular result.

* P41 : Goldbach pairs are not unique except for 12.
  It is difficult to deal with this without making the examples less illustrative.

* P50 : Huffman encoding is not unique, but a direct example of what a Huffman encoding
  looks like is too useful not to include.

* P86 : Multiple graph colorings are possible even with the specific algorithm,
  depending on how ties are broken when sorting.

* P91 : Alternative knight's tours exist, and beyond a 5x5 board,
  it is not feasible to enumerate all of them so that we can pick one
  unique tour to output.

* P92 : The graceful labeling is not unique.
  It is difficult to deal with this without making the example less illustrative.

Examples in the following modules are also intentionally omitted from testing.

* Problems.Graphs : The module and examples themselves are fine.
  However, doctest fails on the examples here for some unknown reason.
  This is likely related to changes in GHC; it used to succeed in older versions.
  doctest does succeed with a standalone manual run
  (`stack exec doctest src/Problems/Graphs.hs`).
-}
