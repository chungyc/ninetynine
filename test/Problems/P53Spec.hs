{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P53Spec (spec) where

import           Data.List             (nub, singleton)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import qualified Data.Set              as Set
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
  modifyMaxSize (const 20) $
  describe name $ do

  it "proves trivial True" $ do
    isTheorem [] (Value True) `shouldBe` True

  it "does not prove trival False" $ do
    isTheorem [] (Value False) `shouldBe` False

  it "proves trival true statement" $ do
    isTheorem [] (Disjoin [Variable "X", Complement $ Variable "X"]) `shouldBe` True

  it "does not prove trival false statement" $ do
    isTheorem [] (Conjoin [Variable "X", Complement $ Variable "X"]) `shouldBe` False

  prop "is not true for no axioms and a single variable conjecture" $
    \s -> isTheorem [] (Variable s) `shouldBe` False

  prop "proves axiom" $
    \(Theory theory) ->
      not (null theory) ==>
      withConsistent theory $
      forAll (elements theory) $ \conjecture ->
      isTheorem theory conjecture `shouldBe` True

  xdescribe "Pending" $ do
    prop "variables satisfying axioms must satisfy theorem" $
      \(Theory theory) -> \(Conjecture conjecture) ->
        forAll (assignmentsFor $ Disjoin [ conjecture, Conjoin theory ]) $ \vs ->
        evaluateFormula vs (Conjoin theory) ==>
        isTheorem theory conjecture ==>
        evaluateFormula vs conjecture `shouldBe` True

    prop "is true for theorem" $ mapSize (const 10) $
      \(Theory theory) -> \(Conjecture conjecture) ->
        isRealTheorem theory conjecture ==>
        isTheorem theory conjecture `shouldBe` True

    prop "is not true for conjecture with unknown variable" $
      \(Theory theory) -> \(Conjecture conjecture) ->
        not ((Set.fromList $ variables conjecture)
             `Set.isSubsetOf`
             (Set.fromList $ concatMap variables theory)) ==>
        not (isTheorem [] conjecture) ==>
        isTheorem theory conjecture `shouldBe` False

    prop "does not prove contradiction with consistent axioms" $
      \(Theory theory) -> \s ->
        not (isTheorem theory $ Value False) ==>
        isTheorem theory (Conjoin [Variable s, Complement $ Variable s]) `shouldBe` False

    where withConsistent theory f =
            forAll (assignmentsFor $ Conjoin theory) $ \vs ->
            evaluateFormula vs (Conjoin $ theory) ==> f

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

  it "isTheorem [ X, !X ] (X | !X)" $ do
    isTheorem [ Variable "X", Complement $ Variable "X" ] (Disjoin [Variable "X", Complement $ Variable "X"]) `shouldBe` True

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

newtype Conjecture = Conjecture Formula deriving (Eq, Show)

instance Arbitrary Conjecture where
  arbitrary = scale (`div` 2) $ Conjecture <$> formulas
  shrink (Conjecture c) = map Conjecture $ shrinkFormula c

-- Generate boolean formulas.
formulas :: Gen Formula
formulas = sized $ gen
  where gen n
          | n < 2 = frequency [ (10, Value <$> arbitrary)
                              , (10, Variable . singleton <$> choose ('A','Z'))
                              , (5, Complement <$> formulas)
                              , (1, Disjoin <$> listOf formulas)
                              , (1, Conjoin <$> listOf formulas)
                              ]
          | otherwise = frequency [ (1, Value <$> arbitrary)
                                  , (1, Variable . singleton <$> choose ('A','Z'))
                                  , (5, scale (subtract 1) $ Complement <$> formulas)
                                  , (10, scale (`div` 2) $ Disjoin <$> listOf formulas)
                                  , (10, scale (`div` 2) $ Conjoin <$> listOf formulas)
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

-- Verifies that the conjecture is a theorem for the theory by brute force.
-- Checks that all possible assigments of variables that are consistent
-- with the theory makes the conjecture true.
isRealTheorem :: [Formula] -> Formula -> Bool
isRealTheorem theory conjecture = all isTrue assignments
  where vars = nub $ variables conjecture ++ concatMap variables theory
        assignments = filter isPossible $ mappings vars [Map.empty]
        isPossible m = evaluateFormula m $ Conjoin theory
        isTrue m = evaluateFormula m conjecture

mappings :: [String] -> [Map String Bool] -> [Map String Bool]
mappings [] m = m
mappings (x:xs) ms = do
  m <- ms
  v <- [False, True]
  mappings xs $ return $ Map.insert x v m
