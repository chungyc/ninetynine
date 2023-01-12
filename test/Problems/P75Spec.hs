{-|
Copyright: Copyright (C) 2023 Yoo Chung
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

properties :: (String -> Maybe (Integer, (Integer, Integer))) -> String -> Spec
properties maybeGoldbach name = describe name $ do
  prop "fails with non-number input" $ \s ->
    (isNothing (readMaybe s :: Maybe Integer)) ==>
    maybeGoldbach s `shouldBe` Nothing

  prop "fails with number less than or equal to 2" $
    \n -> (n :: Integer) <= 2 ==>
    maybeGoldbach (show n) `shouldBe` Nothing

  prop "fails with odd number" $
    forAll oddNumbers $ \n -> maybeGoldbach (show n) `shouldBe` Nothing

  prop "is same as goldbach with even number" $
    forAll (evenNumbers `suchThat` (>2)) $ \n ->
    maybeGoldbach (show n) `shouldBe` Just (n, goldbach n)

  where oddNumbers = (\n -> n*2+1) <$> arbitrarySizedNatural :: Gen Integer
        evenNumbers = (2*) <$> arbitrarySizedNatural

examples :: Spec
examples = describe "Examples" $ do
  it "maybeGoldbach \"104\"" $ do
    maybeGoldbach "104" `shouldBe` Just (104, (3,101))

  it "maybeGoldbach \"not a number\"" $ do
    maybeGoldbach "not a number" `shouldBe` Nothing

  it "maybeGoldbach \"1\"" $ do
    maybeGoldbach "1" `shouldBe` Nothing

  it "maybeGoldbach \"101\"" $ do
    maybeGoldbach "101" `shouldBe` Nothing

  where maybeGoldbach = Problem.maybeGoldbach

spec :: Spec
spec = parallel $ do
  properties Problem.maybeGoldbach "maybeGoldbach"
  properties Problem.maybeGoldbach' "maybeGoldbach'"
  examples
  describe "From solutions" $ do
    properties Solution.maybeGoldbach "maybeGoldbach"
