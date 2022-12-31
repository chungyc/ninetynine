{-|
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P75Spec (spec) where

import           Data.Maybe            (isNothing)
import qualified Problems.P75          as Problem
import           Solutions.P40         (goldbach)
import qualified Solutions.P75         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Text.Read             (readMaybe)

properties :: (String -> Maybe (Int, (Int, Int))) -> String -> Spec
properties maybeGoldbach name = describe name $ do
  prop "fails with non-number input" $
    \s -> (isNothing (readMaybe s :: Maybe Int)) ==> maybeGoldbach s `shouldBe` Nothing

  prop "fails with number less than or equal to 2" $
    \n -> (n :: Int) <= 2 ==> maybeGoldbach (show n) `shouldBe` Nothing

  prop "fails with odd number" $
    \n -> odd (n :: Int) ==> maybeGoldbach (show n) `shouldBe` Nothing

  prop "is same as goldbach with even number" $
    \(Small n') -> n' > 2 ==>
                   let n = 2 * n'
                   in maybeGoldbach (show n) `shouldBe` Just (n, goldbach n)

examples :: Spec
examples = do
  describe "Examples" $ do
    it "maybeGoldbach \"104\"" $ do
      maybeGoldbach "104" `shouldBe` Just (104, (3,101))

    it "maybeGoldbach \"not a number\"" $ do
      maybeGoldbach "not a number" `shouldBe` Nothing

    it "maybeGoldbach \"1\"" $ do
      maybeGoldbach "1" `shouldBe` Nothing

    it "maybeGoldbach \"101\"" $ do
      maybeGoldbach "101" `shouldBe` Nothing

    it "maybeGoldbach' \"104\"" $ do
      maybeGoldbach' "104" `shouldBe` Just (104, (3,101))

    it "maybeGoldbach' \"not a number\"" $ do
      maybeGoldbach' "not a number" `shouldBe` Nothing

    it "maybeGoldbach' \"1\"" $ do
      maybeGoldbach' "1" `shouldBe` Nothing

    it "maybeGoldbach' \"101\"" $ do
      maybeGoldbach' "101" `shouldBe` Nothing

    where maybeGoldbach = Problem.maybeGoldbach
          maybeGoldbach' = Problem.maybeGoldbach'

spec :: Spec
spec = parallel $ do
  properties Problem.maybeGoldbach "maybeGoldbach"
  examples
  describe "From solutions" $ do
    properties Solution.maybeGoldbach "maybeGoldbach"
