{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P52Spec (spec) where

import           Data.List             (singleton)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Problems.Logic        (Formula (..), evaluateFormula)
import qualified Problems.P52          as Problem
import qualified Solutions.P52         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Formula -> Formula) -> String -> Spec
properties toConjunctiveNormalForm name =
  -- Avoid input sizes that are too large.
  -- Particular inputs have conjunctive normal forms that are exponential in length.
  modifyMaxSize (const 20) $
  describe name $ do

  prop "is in conjunctive normal form" $
    forAll generateFormula $ \f ->
    label ("formula depth " ++ show (formulaDepth f)) $
    toConjunctiveNormalForm f
    `shouldSatisfy` isConjunctiveNormalForm

  prop "is equivalent to original formula" $
    forAll generateFormula $ \f -> forAll (generateAssignments f) $ \m ->
    label ("formula depth " ++ show (formulaDepth f)) $
    toConjunctiveNormalForm f
    `shouldSatisfy` (==) (evaluateFormula m f) . evaluateFormula m

examples :: Spec
examples = describe "Examples" $ do
  test "toConjunctiveNormalForm $ Value True" $
    Value True

  test "toConjunctiveNormalForm $ !(X | Y)" $
    Complement $ Disjoin [Variable "X", Variable "Y"]

  test "toConjunctiveNormalForm $ X | (!Y & Z)" $
    Disjoin [Variable "X", Conjoin [Complement $ Variable "Y", Variable "Z"]]

  where toConjunctiveNormalForm = Problem.toConjunctiveNormalForm
        test name formula = describe name $ do
          it "is conjunctiveNormalForm $ Value True" $ do
            toConjunctiveNormalForm formula `shouldSatisfy` isConjunctiveNormalForm

          prop "is equivalent to original formula" $ withMaxSuccess 16 $
            forAll (generateAssignments formula) $ \m ->
            toConjunctiveNormalForm formula
            `shouldSatisfy` (==) (evaluateFormula m formula) . evaluateFormula m

spec :: Spec
spec = parallel $ do
  properties Problem.toConjunctiveNormalForm "toConjunctiveNormalForm"
  examples
  describe "From solutions" $ do
    properties Solution.toConjunctiveNormalForm "toConjunctiveNormalForm"

isConjunctiveNormalForm :: Formula -> Bool
isConjunctiveNormalForm (Conjoin fs) = all isFlatDisjunction fs
isConjunctiveNormalForm _            = False

isFlatDisjunction :: Formula -> Bool
isFlatDisjunction (Disjoin fs) = all isLiteral fs
isFlatDisjunction _            = False

isLiteral :: Formula -> Bool
isLiteral (Value _)                 = True
isLiteral (Variable _)              = True
isLiteral (Complement (Value _))    = True
isLiteral (Complement (Variable _)) = True
isLiteral _                         = False

formulaDepth :: Formula -> Int
formulaDepth (Disjoin [])   = 1
formulaDepth (Conjoin [])   = 1
formulaDepth (Disjoin fs)   = 1 + (maximum $ map formulaDepth fs)
formulaDepth (Conjoin fs)   = 1 + (maximum $ map formulaDepth fs)
formulaDepth (Complement f) = formulaDepth f
formulaDepth _              = 1

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
