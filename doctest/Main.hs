import           Test.DocTest

main :: IO ()
main = do
  doctest $ "--fast" : stateless
  doctest $ toReload
  doctest $ "--fast" : solutions

mapFiles :: [String] -> [String]
mapFiles = map ("src/Problems/" ++)

-- | Deterministic examples with no state or local definitions to reset.
stateless :: [String]
stateless = mapFiles
  [ "Graphs.hs"
  , "P01.hs", "P02.hs", "P03.hs", "P04.hs", "P05.hs"
  , "P06.hs", "P07.hs", "P08.hs", "P09.hs", "P10.hs"
  , "P11.hs", "P12.hs", "P13.hs", "P14.hs", "P15.hs"
  , "P16.hs", "P17.hs"
  , "P31.hs", "P32.hs", "P33.hs", "P34.hs", "P35.hs"
  , "P36.hs", "P39.hs"
  , "P46.hs", "P49.hs"
  , "P54.hs", "P55.hs"
  , "P56.hs"
  ]

-- | Deterministic examples with local definitions to reset.
toReload :: [String]
toReload = mapFiles [ "P48.hs", "P57.hs", "P81.hs", "P82.hs", "P90.hs" ]

-- Examples in solutions.
solutions :: [String]
solutions = map ("src/Solutions/" ++) $ [ "P49.hs" ]
