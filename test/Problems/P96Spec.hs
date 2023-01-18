{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P96Spec (spec) where

import           Data.List             (singleton)
import qualified Problems.P96          as Problem
import qualified Solutions.P96         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (String -> Bool) -> String -> Spec
properties isIdentifier name = describe name $ do
  prop "is true for legal identifiers" $ \(Identifier s) ->
    isIdentifier s `shouldBe` True

  prop "is false when starting with underscore" $ \(Identifier s) ->
    isIdentifier ('_':s) `shouldBe` False

  prop "is false when starting with digit" $
    \(Identifier s) -> forAll digits $ \n ->
      isIdentifier (n:s) `shouldBe` False

  prop "is false when ending with underscore" $ \(Identifier s) ->
    isIdentifier (s ++ "_") `shouldBe` False

  prop "is false when underscores are consecutive" $
    \(Identifier s) -> forAll (splitsOf s) $ \(front, back) ->
      isIdentifier (front ++ "__" ++ back) `shouldBe` False

  prop "is false when invalid characters are included" $ \(Identifier s) c ->
    forAll (splitsOf s) $ \(front, back) ->
    c `notElem` (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['_']) ==>
    isIdentifier (front ++ [c] ++ back) `shouldBe` False

examples :: Spec
examples = describe "Examples" $ do
    it "isIdentifier \"this_is_a_long_identifier\"" $ do
      isIdentifier "this_is_a_long_identifier" `shouldBe` True

    it "isIdentifier \"This_ends_in_an_underscore_\"" $ do
      isIdentifier "This_ends_in_an_underscore_" `shouldBe` False

    it "isIdentifier \"This__has__two__consecutive__underscores\"" $ do
      isIdentifier "This__has__two__consecutive__underscores" `shouldBe` False

    it "isIdentifier \"1234\"" $ do
      isIdentifier "1234" `shouldBe` False

    it "isIdentifier \"_legal_in_many_other_languages\"" $ do
      isIdentifier "_legal_in_many_other_languages" `shouldBe` False

    it "isIdentifier \"Fibonacci_sequence_is_1_1_2_3_5_8_13_21_ad_infinitum\"" $ do
      isIdentifier "Fibonacci_sequence_is_1_1_2_3_5_8_13_21_ad_infinitum" `shouldBe` True

  where isIdentifier = Problem.isIdentifier

spec :: Spec
spec = parallel $ do
  properties Problem.isIdentifier "isIdentifier"
  examples
  describe "From solutions" $ do
    properties Solution.isIdentifier "isIdentifier"

-- | Arbitrary legal identifier.
newtype Identifier = Identifier String deriving Show

instance Arbitrary Identifier where
  arbitrary = Identifier <$> identifiers

-- | Generates a legal identifer.
--
-- A direct translation of the syntax diagram into generator form.
identifiers :: Gen String
identifiers = fromFirstLetter

fromFirstLetter :: Gen String
fromFirstLetter = do
  n <- getSize
  frequency [ (n, (:) <$> letters <*> extended), (1, singleton <$> letters) ]

extended :: Gen String
extended = frequency [ (10, extended'), (1, fromUnderscore) ]

fromUnderscore :: Gen String
fromUnderscore = (:) '_' <$> extended'

extended' :: Gen String
extended' = frequency [ (5, fromLetter), (1, fromDigit) ]

fromLetter :: Gen String
fromLetter = (:) <$> letters <*> extended''

fromDigit :: Gen String
fromDigit = (:) <$> digits <*> extended''

extended'' :: Gen String
extended'' = do
  n <- getSize
  frequency [ (n, resize (n-1) extended), (1, pure "") ]

letters :: Gen Char
letters = elements $ ['A'..'Z'] ++ ['a'..'z']

digits :: Gen Char
digits = elements ['0'..'9']

-- | Generates a split into two strings for a given string.
splitsOf :: String -> Gen (String, String)
splitsOf s = do
  k <- chooseInt (0, length s - 1)
  return $ splitAt k s
