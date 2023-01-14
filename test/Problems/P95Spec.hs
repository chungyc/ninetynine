{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P95Spec (spec) where

import qualified Problems.P95          as Problem
import qualified Solutions.P95         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Int -> String) -> String -> Spec
properties fullWords name = describe name $ do
  context "with single digit" $
    let test n word =
          prop (show n ++ " is " ++ word) $
            fullWords n `shouldBe` word
    in do test 0 "zero"
          test 1 "one"
          test 2 "two"
          test 3 "three"
          test 4 "four"
          test 5 "five"
          test 6 "six"
          test 7 "seven"
          test 8 "eight"
          test 9 "nine"

  prop "concatenates from smaller numbers" $
    \(Positive x) -> \(NonNegative y) ->
      counterexample (show $ concatenate x y) $
      fullWords (concatenate x y) `shouldBe` fullWords x ++ "-" ++ fullWords y

examples :: Spec
examples = describe "Examples" $ do
  it "fullWords 175" $ do
    fullWords (175 :: Int) `shouldBe` "one-seven-five"

  where fullWords = Problem.fullWords

spec :: Spec
spec = parallel $ do
  properties Problem.fullWords "fullWords"
  examples
  describe "From solutions" $ do
    properties Solution.fullWords "fullWords"

-- Combine two number together such that the string representation
-- of the combined number is a concatenation of the string representation
-- of the individual numbers.
--
-- >>> concatenate 123 456
-- "123456"
concatenate :: Integral a => Show a => a -> a -> a
concatenate x y
  | x > 0, y == 0 = 10 * x
  | x > 0, y > 0 = enlarge x y + y
  | otherwise = error $ "cannot concatenate " ++ show x ++ " and " ++ show y
  where enlarge n 0 = n
        enlarge n m = enlarge (10 * n) (m `div` 10)
