{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P53Spec (spec) where

import           Problems.Logic        (Formula (..))
import qualified Problems.P53          as Problem
import qualified Solutions.P53         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Formula] -> Formula -> Bool) -> String -> Spec
properties isTheorem name = describe name $ do
  prop "proves theorem" $
    \(Theory theory) -> \(Theorem theorem) -> isTheorem theory theorem `shouldBe` False

  prop "is pending" $
    pending

examples :: Spec
examples = describe "Examples" $ do
  it "isTheorem [ X, Y, Z ] (X | Y)" $ do
    isTheorem
      [ Variable "X", Variable "Y", Variable "Z" ]
      (Disjoin [ Variable "X", Variable "Y" ])
    `shouldBe` True

  it "isTheorem [ !X | Y, !Y | Z ] (!X | Z)" $ do
    isTheorem
      [ Disjoin [ Complement $ Variable "X", Variable "Y" ]
      , Disjoin [ Complement $ Variable "Y", Variable "Z" ]
      ]
      (Disjoin [ Complement $ Variable "X", Variable "Z" ])
    `shouldBe` True

  it "isTheorem [ X ] Y" $ do
    isTheorem [ Variable "X" ] (Variable "Y") `shouldBe` False

  where isTheorem = Problem.isTheorem

spec :: Spec
spec = parallel $ do
  properties Problem.isTheorem "isTheorem"
  examples
  describe "From solutions" $ do
    properties Solution.isTheorem "isTheorem"

newtype Theory = Theory [Formula] deriving (Eq, Show)

instance Arbitrary Theory where
  arbitrary = undefined

newtype Theorem = Theorem Formula deriving (Eq, Show)

instance Arbitrary Theorem where
  arbitrary = undefined
