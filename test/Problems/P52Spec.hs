{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P52Spec (spec) where

import           Problems.Logic            (Formula (..), evaluateFormula)
import           Problems.Logic.Arbitrary  ()
import           Problems.Logic.QuickCheck (assignmentsFor)
import qualified Problems.P52              as Problem
import qualified Solutions.P52             as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Formula -> Formula) -> String -> Spec
properties toConjunctiveNormalForm name = describe name $ modifyMaxSize (const 15) $ do
  prop "is in conjunctive normal form" $
    \f -> label ("formula depth " ++ show (formulaDepth f)) $
          toConjunctiveNormalForm f
          `shouldSatisfy` isConjunctiveNormalForm

  prop "is equivalent to original formula" $
    \f -> forAll (assignmentsFor f) $ \m ->
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
            forAll (assignmentsFor formula) $ \m ->
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
