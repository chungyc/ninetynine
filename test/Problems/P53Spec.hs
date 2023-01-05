{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P53Spec (spec) where

import           Data.List             (singleton, subsequences)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           GHC.Generics          (Generic)
import           Problems.Logic        (Formula (..))
import qualified Problems.P53          as Problem
import qualified Solutions.P53         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Formula] -> Formula -> Bool) -> String -> Spec
properties isTheorem name = xdescribe name $ do
  prop "proves axiom" $
    \(Theory theory) -> forAll (elements theory) $ \conjecture ->
      isTheorem theory conjecture `shouldBe` True

  prop "must have any assignment of variables satisfy theorem" $
    \(Theory theory) -> forAll (generateAssignments $ Conjoin theory) $ \_ ->
      pending

  prop "is true for theorem" $
    pending

  prop "is not true for conjecture with unknown variable" $
    pending

  prop "does not prove contradiction" $
    pending

examples :: Spec
examples = xdescribe "Examples" $ do
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

newtype Theory = Theory [Formula] deriving (Eq, Show, Generic)

instance Arbitrary Theory where
  arbitrary = scale (`div` 2) $ Theory <$> listOf generateFormula
  shrink (Theory []) = []
  shrink (Theory fs) = map Theory $ subsequences fs

generateFormula :: Gen Formula
generateFormula = sized $ gen
  where gen n
          | n < 2 = frequency [ (10, Value <$> arbitrary)
                              , (10, Variable . singleton <$> choose ('A','Z'))
                              , (5, Complement <$> generateFormula)
                              , (1, Disjoin <$> listOf generateFormula)
                              , (1, Conjoin <$> listOf generateFormula)
                              ]
          | otherwise = frequency [ (1, Value <$> arbitrary)
                                  , (1, Variable . singleton <$> choose ('A','Z'))
                                  , (5, scale (subtract 1) $ Complement <$> generateFormula)
                                  , (10, scale (`div` 2) $ Disjoin <$> listOf generateFormula)
                                  , (10, scale (`div` 2) $ Conjoin <$> listOf generateFormula)
                                  ]

generateAssignments :: Formula -> Gen (Map String Bool)
generateAssignments f = assign vs <$> vectorOf (length vs) arbitrary
  where assign vars values = Map.fromList $ zip vars values
        vs = variables f
        variables (Value _)       = []
        variables (Variable s)    = [s]
        variables (Complement f') = variables f'
        variables (Disjoin fs)    = concatMap variables fs
        variables (Conjoin fs)    = concatMap variables fs
