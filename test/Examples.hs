{-|
Description: Tests for the examples included with the documentation
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Main (main) where

import           Data.List                     (isInfixOf)
import           System.Console.GetOpt
import           System.Environment            (getArgs)
import           Test.DocTest                  (mainFromCabalWithConfig)
import           Test.DocTest.Internal.Options (Config (..), ModuleName,
                                                defaultConfig)
main :: IO ()
main = do
  args <- getArgs
  match <- matchOptions args
  let modules' = match modules
  let config =  defaultConfig { cfgModules = modules' }
  if null modules'
    then return ()
    else mainFromCabalWithConfig "ninetynine" config

-- | Modules with examples to be tested.
modules :: [ModuleName]
modules = [ "Problems.Crosswords"
          , "Problems.Graphs"
          , "Problems.MultiwayTrees"
          , "Problems.Monads"
          , "Problems.P01"
          , "Problems.P02"
          , "Problems.P03"
          , "Problems.P04"
          , "Problems.P05"
          , "Problems.P06"
          , "Problems.P07"
          , "Problems.P08"
          , "Problems.P09"
          , "Problems.P10"
          , "Problems.P11"
          , "Problems.P12"
          , "Problems.P13"
          , "Problems.P14"
          , "Problems.P15"
          , "Problems.P16"
          , "Problems.P17"
          , "Problems.P18"
          , "Problems.P19"
          , "Problems.P20"
          , "Problems.P21"
          , "Problems.P22"
          , "Problems.P26"
          , "Problems.P27"
          , "Problems.P28"
          , "Problems.P29"
          , "Problems.P30"
          , "Problems.P31"
          , "Problems.P32"
          , "Problems.P33"
          , "Problems.P34"
          , "Problems.P35"
          , "Problems.P36"
          , "Problems.P38"
          , "Problems.P39"
          , "Problems.P40"
          , "Problems.P42"
          , "Problems.P43"
          , "Problems.P44"
          , "Problems.P45"
          , "Problems.P46"
          , "Problems.P47"
          , "Problems.P48"
          , "Problems.P49"
          , "Problems.P53"
          , "Problems.P55"
          , "Problems.P56"
          , "Problems.P57"
          , "Problems.P58"
          , "Problems.P59"
          , "Problems.P60"
          , "Problems.P61"
          , "Problems.P62"
          , "Problems.P63"
          , "Problems.P64"
          , "Problems.P65"
          , "Problems.P66"
          , "Problems.P67"
          , "Problems.P68"
          , "Problems.P69"
          , "Problems.P70"
          , "Problems.P71"
          , "Problems.P72"
          , "Problems.P73"
          , "Problems.P74"
          , "Problems.P75"
          , "Problems.P76"
          , "Problems.P77"
          , "Problems.P78"
          , "Problems.P79"
          , "Problems.P81"
          , "Problems.P82"
          , "Problems.P83"
          , "Problems.P84"
          , "Problems.P85"
          , "Problems.P87"
          , "Problems.P88"
          , "Problems.P89"
          , "Problems.P90"
          , "Problems.P93"
          , "Problems.P94"
          , "Problems.P95"
          , "Problems.P96"
          , "Problems.P97"
          , "Problems.P98"
          , "Problems.P99"
          ]

{-
Examples in the following problems are intentionally omitted from testing.

* P23, P24, P25, P51 : Randomness means we cannot expect deterministic output.
  Even with a fixed pseudo-random number generator, we cannot impose any particular result.

* P41 : Goldbach pairs are not unique except for 12.
  It is difficult to deal with this without making the examples less illustrative.

* P50 : Huffman encoding is not unique, but a direct example of what a Huffman encoding
  looks like is too useful not to include.

* P52 : The conjunctive normal form of boolean formula is often not unique.

* P86 : Multiple graph colorings are possible even with the specific algorithm,
  depending on how ties are broken when sorting.

* P91 : Alternative knight's tours exist, and beyond a 5x5 board,
  it is not feasible to enumerate all of them so that we can pick one
  unique tour to output.

* P92 : The graceful labeling is not unique.
  It is difficult to deal with this without making the example less illustrative.
-}

data Flag = Match String deriving Show

options :: [OptDescr Flag]
options = [ Option [] ["match"] (ReqArg Match "match") "substring match on file name" ]

-- | Support @--match@ option that is also supported by Hspec.
matchOptions :: [String] -> IO ([String] -> [String])
matchOptions argv =
  case getOpt Permute options argv of
    ([Match m], _, _) -> return $ filter (isInfixOf m)
    _                 -> return $ id
