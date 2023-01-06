{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P53Spec (spec) where

import           Data.List             (singleton)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Problems.Logic        (Formula (..), evaluateFormula)
import qualified Problems.P53          as Problem
import qualified Solutions.P53         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Formula] -> Formula -> Bool) -> String -> Spec
properties isTheorem name =
  -- Avoid sizes that are too large.
  -- An exponential number of applying the resolution rule may be necessary.
  modifyMaxSize (const 16) $
  describe name $ do

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
newtype Theory = Theory [Formula] deriving (Eq, Show)

instance Arbitrary Theory where
  arbitrary = scale (`div` 2) $ Theory <$> listOf formulas
  shrink (Theory fs) = map Theory (shrinkList shrinkFormula fs)

-- An arbitrary formula that can be used as a conjecture to prove.
newtype Conjecture = Conjecture Formula deriving (Eq, Show)

instance Arbitrary Conjecture where
  arbitrary = scale (`div` 2) $ Conjecture <$> formulas
  shrink (Conjecture c) = map Conjecture $ shrinkFormula c

-- Generate boolean formulas.
formulas :: Gen Formula
formulas = sized $ gen
  where gen n
          | n < 2 = frequency [ (1, Value <$> arbitrary)
                              , (1000, Variable . singleton <$> choose ('A','Z'))
                              , (500, Complement <$> formulas)
                              , (100, Disjoin <$> listOf1 formulas)
                              , (100, Conjoin <$> listOf1 formulas)
                              ]
          | otherwise = frequency [ (1, Value <$> arbitrary)
                                  , (100, Variable . singleton <$> choose ('A','Z'))
                                  , (500, scale (subtract 1) $ Complement <$> formulas)
                                  , (1000, scale (`div` 2) $ Disjoin <$> listOf1 formulas)
                                  , (1000, scale (`div` 2) $ Conjoin <$> listOf1 formulas)
                                  ]

shrinkFormula :: Formula -> [Formula]
shrinkFormula (Value _)      = []
shrinkFormula (Variable _)   = []
shrinkFormula (Complement f) = [f] ++ map Complement (shrinkFormula f)
shrinkFormula (Disjoin [])   = []
shrinkFormula (Conjoin [])   = []
shrinkFormula (Disjoin fs)   = fs ++ map Disjoin (shrinkList shrinkFormula fs)
shrinkFormula (Conjoin fs)   = fs ++ map Conjoin (shrinkList shrinkFormula fs)

-- Generate variable assignments for a boolean formula.
assignmentsFor :: Formula -> Gen (Map String Bool)
assignmentsFor f = assign vs <$> vectorOf (length vs) arbitrary
  where assign vars values = Map.fromList $ zip vars values
        vs = variables f

-- List of all variable names in a boolean formula.
variables :: Formula -> [String]
variables (Value _)       = []
variables (Variable s)    = [s]
variables (Complement f') = variables f'
variables (Disjoin fs)    = concatMap variables fs
variables (Conjoin fs)    = concatMap variables fs
