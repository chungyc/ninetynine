import           Test.DocTest

main :: IO ()
main = do
  doctest $ "--fast" : stateless
  doctest $ stateful
  doctest $ background
  doctest $ "--fast" : solutions

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
  , "P31.hs", "P32.hs", "P33.hs", "P34.hs", "P35.hs"
  , "P36.hs", "P39.hs", "P40.hs"
  , "P46.hs", "P49.hs"
  , "P54.hs", "P55.hs"
  , "P56.hs", "P57.hs", "P59.hs", "P60.hs"
  , "P63.hs", "P64.hs", "P65.hs"
  , "P70.hs"
  , "P71.hs", "P72.hs", "P73.hs"
  , "P83.hs", "P85.hs"
  , "P95.hs", "P97.hs"
  ]

-- | Deterministic examples with local definitions to reset.
stateful :: [String]
stateful = mapFiles [ "P48.hs", "P57.hs", "P61.hs", "P62.hs", "P66.hs",
                      "P81.hs", "P82.hs", "P84.hs", "P90.hs" ]

-- | Deterministic examples in background modules.
-- Run separately from problem modules to avoid type mismatches.
background :: [String]
background = mapFiles ["Graphs.hs", "MultiwayTrees.hs"]

-- Examples in solutions.
solutions :: [String]
solutions = map ("src/Solutions/" ++) $ [ "P49.hs" ]

{-
Examples in the following problems are intentionally omitted from testing.

* P23, P24, P25 : Randomness means we cannot expect deterministic output.
  Even with a fixed pseudo-random number generator, we cannot impose any particular result.

* P86 : Multiple graph colorings are possible even with the specific algorithm,
  depending on how ties are broken when sorting.

* P91 : Alternative knight's tours exist, and beyond a 5x5 board,
  it is not feasible to enumerate all of them so that we can pick one
  unique tour to output.
-}
