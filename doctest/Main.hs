import           Test.DocTest

main :: IO ()
main = doctest $
  "--fast" :
  map ("src/Problems/" ++) [ "P01.hs", "P02.hs", "P03.hs", "P04.hs", "P05.hs"
                           , "P06.hs", "P07.hs", "P08.hs", "P09.hs", "P10.hs"
                           , "P11.hs", "P12.hs", "P13.hs", "P14.hs", "P15.hs"
                           , "P31.hs", "P32.hs"
                           ]
