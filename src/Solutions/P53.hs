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
  | Nothing <- m = False
  | Just matched <- m = case (resolve clauses matched) of
      Nothing       -> True
      Just resolved -> refute resolved
  where m = match clauses

type Match = ((Formula, Set Formula), (Formula, Set Formula))

match :: Set (Set Formula) -> Maybe Match
match clauses
  | [] <- matches = Nothing
  | (m:_) <- matches = Just m
  where matches = do
          let clauseList = Set.toList clauses
          clause <- clauseList
          clause' <- clauseList
          x <- Set.toList clause
          let x' = complement x
          guard $ Set.member x' clause'
          return ((x, clause), (x', clause'))

resolve :: Set (Set Formula) -> Match -> Maybe (Set (Set Formula))
resolve clauses ((v, c), (v', c'))
  | Set.null c'' = Nothing
  | isTautology c'' = Just clauses'
  | otherwise = Just $ Set.insert c'' $ clauses'
  where c'' = Set.delete v' $ Set.delete v $ c `Set.union` c'
        clauses' = Set.delete c $ Set.delete c' $ clauses

isTautology :: Set Formula -> Bool
isTautology s = any (\x -> complement x `Set.member` s) s

complement :: Formula -> Formula
complement (Complement x) = x
complement x              = Complement x
