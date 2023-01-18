{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.Logic.QuickCheck (formulas, shrinkFormula, assignmentsFor) where

import           Data.List       (singleton)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Problems.Logic
import           Test.QuickCheck

-- Generate boolean formulas.
formulas :: Gen Formula
formulas = sized gen
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

-- Shrink a boolean formula.
shrinkFormula :: Formula -> [Formula]
shrinkFormula (Value _)      = []
shrinkFormula (Variable _)   = []
shrinkFormula (Complement f) = f : map Complement (shrinkFormula f)
shrinkFormula (Disjoin [])   = []
shrinkFormula (Conjoin [])   = []
shrinkFormula (Disjoin fs)   = fs ++ map Disjoin (shrinkList shrinkFormula fs)
shrinkFormula (Conjoin fs)   = fs ++ map Conjoin (shrinkList shrinkFormula fs)

-- | Generate variable assignments for a boolean formula.
assignmentsFor :: Formula -> Gen (Map String Bool)
assignmentsFor f = assign vs <$> vectorOf (length vs) arbitrary
  where assign vars values = Map.fromList $ zip vars values
        vs = variables f
        variables (Value _)       = []
        variables (Variable s)    = [s]
        variables (Complement f') = variables f'
        variables (Disjoin fs)    = concatMap variables fs
        variables (Conjoin fs)    = concatMap variables fs
