{- |
Description: Resolution rule
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P53" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P53 (isTheorem) where

import           Data.Set       (Set)
import qualified Data.Set       as Set
import           Problems.Logic (Formula (..))
import           Problems.P52   (toConjunctiveNormalForm)

isTheorem :: [Formula] -> Formula -> Bool
isTheorem axioms conjecture = null $ resolve clauses
  where aggregate = Conjoin $ Complement conjecture : axioms
        cnf = toConjunctiveNormalForm aggregate
        clauses = toClauses cnf

toClauses :: Formula -> [Set Formula]
toClauses (Conjoin fs) = map toSet fs
toClauses f            = error $ "not conjunctive normal form: " ++ show f

toSet :: Formula -> Set Formula
toSet (Disjoin fs) = Set.fromList fs
toSet f            = error $ "not part of conjunctive normal form: " ++ show f

resolve :: [Set Formula] -> [Set Formula]
resolve = undefined
