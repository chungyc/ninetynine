{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P96Spec where

import           Control.Monad
import qualified Problems.P96          as Problem
import qualified Solutions.P96         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (String -> Bool) -> String -> Spec
properties isIdentifier name = do
  describe name $ do
    prop "is true for legal identifiers" $
      \(Identifier s) -> isIdentifier s `shouldBe` True

    prop "is false when starting with underscore" $
      \(Identifier s) -> isIdentifier ('_':s) `shouldBe` False

    prop "is false when starting with digit" $
      \(Identifier s) -> forAll (choose ('0','9')) $ \n ->
        isIdentifier (n:s) `shouldBe` False

    prop "is false when ending with underscore" $
      \(Identifier s) -> isIdentifier (s ++ "_") `shouldBe` False

    prop "is false when underscores are consecutive" $
      \(Identifier s) -> forAll (chooseInt (0, length s - 1)) $ \k ->
        isIdentifier (take k s ++ "__" ++ drop k s) `shouldBe` False

    prop "is false when invalid characters are included" $
      \(Identifier s) -> forAll (chooseInt (0, length s - 1)) $ \k -> \c ->
        c `notElem` (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['_']) ==>
        isIdentifier (take k s ++ [c] ++ drop k s) `shouldBe` False

examples :: Spec
examples = do
  describe "Examples" $ do
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

-- | Arbitrary legal identifiers.
newtype Identifier = Identifier String deriving Show

instance Arbitrary Identifier where
  arbitrary = liftM Identifier genIdentifier

-- | A direct translation of the syntax diagram into generator form.
genIdentifier :: Gen String
genIdentifier = genFirstLetter

genFirstLetter :: Gen String
genFirstLetter = do
  letter <- elements letters
  n <- getSize
  frequency [ (n, liftM ((:) letter) genExtended)
            , (1, return $ [letter])
            ]

genExtended :: Gen String
genExtended = frequency [ (10, genExtended')
                        , (1, genUnderscore)
                        ]

genUnderscore :: Gen String
genUnderscore = liftM ((:) '_') genExtended'

genExtended' :: Gen String
genExtended' = frequency [ (5, genLetter)
                         , (1, genDigit)
                         ]

genLetter :: Gen String
genLetter = do
  letter <- elements letters
  liftM ((:) letter) genExtended''

genDigit :: Gen String
genDigit = do
  digit <- elements ['0'..'9']
  liftM ((:) digit) genExtended''

genExtended'' :: Gen String
genExtended'' = do
  n <- getSize
  frequency [ (n, resize (n-1) $ genExtended)
            , (1, return $ "")
            ]

letters :: [Char]
letters = ['A'..'Z'] ++ ['a'..'z']
