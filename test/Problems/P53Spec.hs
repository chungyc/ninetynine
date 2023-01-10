{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P53Spec (spec) where

import           Problems.Logic            (Formula (..), evaluateFormula)
import           Problems.Logic.Arbitrary  ()
import           Problems.Logic.QuickCheck (assignmentsFor)
import qualified Problems.P53              as Problem
import qualified Solutions.P53             as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Formula] -> Formula -> Bool) -> String -> Spec
properties isTheorem name = describe name $ modifyMaxSize (const 16) $ do
  it "trivially proves true" $ do
    isTheorem [] (Value True) `shouldBe` True

  it "does not trivially prove false" $ do
    isTheorem [] (Value False) `shouldBe` False

  prop "proves axiom" $
    \(Theory theory) ->
      not (null theory) ==>
      forAll (elements theory) $ \conjecture ->
      within timeout $
      isTheorem theory conjecture `shouldBe` True

  prop "consistent axioms never prove false" $
    \(Theory theory) -> forAll (assignmentsFor $ Conjoin theory) $ \vs ->
      evaluateFormula vs (Conjoin theory) ==>
      within timeout $
      isTheorem theory (Value False) `shouldBe` False

  prop "contradictions prove everything" $
    \(Conjecture conjecture) ->
      within timeout $
      isTheorem [Variable "X", Complement $ Variable "X"] conjecture `shouldBe` True

  prop "proves or disproves" $
    \(Theory theory) -> \(Conjecture conjecture) ->
      within timeout $
      total $ isTheorem theory conjecture

  where timeout = 10 * 1000 * 1000  -- ten seconds

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

  it "isTheorem [ X, !X ] False" $ do
    isTheorem [ Variable "X", Complement $ Variable "X" ] (Value False) `shouldBe` True

  where isTheorem = Problem.isTheorem

spec :: Spec
spec = parallel $ do
  properties Problem.isTheorem "isTheorem"
  examples
  describe "From solutions" $ do
    properties Solution.isTheorem "isTheorem"

-- A collection of boolean formulas forming a set of axioms.
newtype Theory = Theory [Formula] deriving (Show)

instance Arbitrary Theory where
  arbitrary = scale (`div` 2) $ Theory <$> listOf arbitrary
  shrink (Theory fs) = map Theory (shrinkList shrink fs)

-- An arbitrary formula that can be used as a conjecture to prove.
newtype Conjecture = Conjecture Formula deriving (Show)

instance Arbitrary Conjecture where
  arbitrary = scale (`div` 2) $ Conjecture <$> arbitrary
  shrink (Conjecture c) = map Conjecture $ shrink c
