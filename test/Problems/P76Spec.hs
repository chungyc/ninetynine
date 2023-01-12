{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P76Spec (spec) where

import           Data.Maybe            (isNothing)
import qualified Problems.P76          as Problem
import           Solutions.P40         (goldbach)
import qualified Solutions.P76         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Text.Read             (readMaybe)

properties :: (String -> Either String (Integer, (Integer, Integer))) -> String -> Spec
properties eitherGoldbach name = describe name $ do
  prop "fails with non-number input" $
    \s -> (isNothing (readMaybe s :: Maybe Integer)) ==>
    eitherGoldbach s `shouldBe` Left "not a number"

  prop "fails with number less than or equal to 2" $
    \n -> (n :: Integer) <= 2 ==>
    eitherGoldbach (show n) `shouldBe` Left "not greater than 2"

  prop "fails with odd number" $
    forAll (oddNumbers `suchThat` (>2)) $ \n ->
    eitherGoldbach (show n) `shouldBe` Left "not an even number"

  prop "is same as goldbach with even number" $
    forAll (evenNumbers `suchThat` (>2)) $ \n ->
    eitherGoldbach (show n) `shouldBe` Right (n, goldbach n)

  where oddNumbers = (\n -> n*2+1) <$> arbitrarySizedNatural :: Gen Integer
        evenNumbers = (2*) <$> arbitrarySizedNatural

examples :: Spec
examples = describe "Examples" $ do
  it "eitherGoldbach \"104\"" $ do
    eitherGoldbach "104" `shouldBe` Right (104, (3,101))

  it "eitherGoldbach \"this is not a number\"" $ do
    eitherGoldbach "this is not a number" `shouldBe` Left "not a number"

  it "eitherGoldbach \"1\"" $ do
    eitherGoldbach "1" `shouldBe` Left "not greater than 2"

  it "eitherGoldbach \"101\"" $ do
    eitherGoldbach "101" `shouldBe` Left "not an even number"

  where eitherGoldbach = Problem.eitherGoldbach

spec :: Spec
spec = parallel $ do
  properties Problem.eitherGoldbach "eitherGoldbach"
  examples
  describe "From solutions" $ do
    properties Solution.eitherGoldbach "eitherGoldbach"
