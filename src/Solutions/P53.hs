{- |
Description: Resolution rule
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P53" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P53 (isTheorem) where

import           Control.Monad  (guard)
import           Data.Set       (Set)
import qualified Data.Set       as Set
import           Problems.Logic (Formula (..))
import           Problems.P52   (toConjunctiveNormalForm)

isTheorem :: [Formula] -> Formula -> Bool
isTheorem axioms conjecture = refute $ Set.filter (not . isTautology) clauses
  where axioms' = map purify axioms
        conjecture' = purify conjecture
        aggregate = Conjoin $ Complement conjecture' : axioms'
        clauses = toClauses $ toConjunctiveNormalForm aggregate

purify :: Formula -> Formula
purify (Value True)   = true
purify (Value False)  = false
purify f@(Variable _) = f
purify (Complement f) = Complement $ purify f
purify (Disjoin [])   = false
purify (Conjoin [])   = true
purify (Disjoin fs)   = Disjoin $ map purify fs
purify (Conjoin fs)   = Conjoin $ map purify fs

true :: Formula
true = Disjoin [ x, Complement x ]
  where x = Variable "X"

false :: Formula
false = Conjoin [ x, Complement x ]
  where x = Variable "X"

toClauses :: Formula -> Set (Set Formula)
toClauses (Conjoin fs) = Set.fromList $ map toSet fs
toClauses f            = error $ "not conjunctive normal form: " ++ show f

toSet :: Formula -> Set Formula
toSet (Disjoin fs) = Set.fromList fs
toSet f            = error $ "not part of conjunctive normal form: " ++ show f

refute :: Set (Set Formula) -> Bool
refute clauses
  | Nothing <- resolved = False
  | Just c <- resolved, Set.null c = True
  | Just c <- resolved, Set.member c clauses = error $ "infinite loop!  " ++ show (c, clauses)
  | Just c <- resolved = refute $ Set.insert c clauses
  where resolved = resolve clauses $ matches clauses

type Match = ((Formula, Set Formula), (Formula, Set Formula))

matches :: Set (Set Formula) -> [Match]
matches clauses = do
  let clauseList = Set.toList clauses
  clause <- clauseList
  clause' <- clauseList
  x <- Set.toList clause
  let x' = complement x
  guard $ Set.member x' clause'
  return ((x, clause), (x', clause'))

resolve :: Set (Set Formula) -> [Match] -> Maybe (Set Formula)
resolve clauses matchings
  | [] <- ms = Nothing
  | (m:_) <- ms = Just $ merge m
  where ms = dropWhile ignored matchings
        merge ((v, c), (v', c')) = Set.delete v $ Set.delete v' $ c `Set.union` c'
        ignored m = let c = merge m
                    in c `Set.member` clauses || isTautology c

isTautology :: Set Formula -> Bool
isTautology s = any (\x -> complement x `Set.member` s) s

complement :: Formula -> Formula
complement (Complement x) = x
complement x              = Complement x
