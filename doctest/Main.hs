import           Test.DocTest

main :: IO ()
main = doctest $
  ["--verbose"] ++
  map ("src/Problems/" ++) ["P01.hs", "P02.hs"]
