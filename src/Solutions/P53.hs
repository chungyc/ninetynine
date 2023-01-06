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

{- | In propositional logic, if a formula \(X\) is always true
when a set of axioms \(\mathrm{S}\) are true,
then \(X\) is said to be /valid/ given \(\mathrm{S}\).

The /resolution rule/ is an inference rule in propositional logic
where two clauses with complementary literals can be merged to produce
another clause.  Specifically, if clauses \(x \vee a_1 \vee \ldots \vee a_m\)
and \(\neg x \vee b_1 \vee \ldots \vee b_n\) are true, then
the clause \(a_1 \vee \ldots \vee a_m \vee b_1 \vee \ldots \vee b_n\)
must also be true.  This is commonly expressed as

\[
x \vee a_1 \vee \ldots \vee a_m \hspace{2em} \neg x \vee b_1 \vee \ldots \vee b_n
\above 1pt
a_1 \vee \ldots \vee a_m \vee b_1 \vee \ldots \vee b_n
\]

The resolution rule can be used with conjunctive normal forms to prove whether or not
a given formula \(X\) is valid given a set of axioms \(\mathrm{S}\).
It is a proof by contradiction, and the procedure is as follows:

1.  Conjunctively connect the formulas in \(\mathrm{S}\)
    and the /negation/ of \(X\) into a single formula \(Y\).

    For example, if the axioms are \(x\) and \(y\), and
    the given formula is \(z\), then \(Y\) would be \(x \wedge y \wedge \neg z\).

    * Note that the resolution rule does not apply to the values of true and false.
      If \(Y\) contains raw values, transform \(Y\) into a form which contains none.

2.  Convert \(Y\) into conjunctive normal form; see "Problems.P52".

3.  If there are two clauses for which the resolution rule can be applied,
    add the new clause to \(Y\) and repeat until an empty clause is inferred
    or if no new clauses can be added.  Filter out tautologies, i.e.,
    clauses which will always be true because it contains both a variable
    and its logical complement.

    *   If the empty clause has been inferred, then false has been inferred
        from \(Y\).  I.e., there is a contradiction, so \(X\) must have been
        valid given the axioms \(\mathrm{S}\).

    *   If no more clauses can be inferred, then \(X\) cannot be inferred to be valid.

Given a list of axioms and a formula, use the resolution rule to determine
whether the formula is valid given the axioms.
-}
isTheorem :: [Formula] -> Formula -> Bool
isTheorem axioms conjecture = refute $ Set.filter (not . isTautology) clauses
  where axioms' = map purify axioms
        conjecture' = purify conjecture
        aggregate = Conjoin $ Complement conjecture' : axioms'
        clauses = toClauses $ toConjunctiveNormalForm aggregate

-- | Replace all values and empty clauses in a formula with
-- equivalent expressions which have no values and no empty clauses.
purify :: Formula -> Formula
purify (Value True)   = true
purify (Value False)  = false
purify f@(Variable _) = f
purify (Complement f) = Complement $ purify f
purify (Disjoin [])   = false
purify (Conjoin [])   = true
purify (Disjoin fs)   = Disjoin $ map purify fs
purify (Conjoin fs)   = Conjoin $ map purify fs

-- An expression which is always true; for use with purify.
true :: Formula
true = Disjoin [ x, Complement x ]
  -- An arbitrary variable.
  --
  -- It does not matter whether the rest of the formula
  -- uses this variable or not.  Either way, the rest
  -- of the formula does not affect the result of this
  -- expression, and vice versa.
  where x = Variable "X"

-- An expression which is always false; for use with purify.
false :: Formula
false = Conjoin [ x, Complement x ]
  -- An arbitrary variable.
  --
  -- It does not matter whether the rest of the formula
  -- uses this variable or not.  Either way, the rest
  -- of the formula does not affect the result of this
  -- expression, and vice versa.
  where x = Variable "X"

-- | Transform a formula in conjunctive normal form into a set of clauses,
-- with each clause being a set of literals.
toClauses :: Formula -> Set (Set Formula)
toClauses (Conjoin fs) = Set.fromList $ map toSet fs
toClauses f            = error $ "not conjunctive normal form: " ++ show f

-- | Transform a disjunction of literals into a set of literals representing a clause.
toSet :: Formula -> Set Formula
toSet (Disjoin fs) = Set.fromList fs
toSet f            = error $ "not part of conjunctive normal form: " ++ show f

-- | Refute the given formula in conjunctive normal form.
-- I.e., derive a false statement, which means contradiction.
refute :: Set (Set Formula) -> Bool
refute clauses
  | Nothing <- resolved = False
  | Just c <- resolved, Set.null c = True
  | Just c <- resolved = refute $ Set.insert c clauses
  where resolved = resolve clauses $ matches clauses

-- | Represents two clauses, each of which has a literal and its complement.
-- Each pair is the matching literal and the clause containing the literal.
type Match = ((Formula, Set Formula), (Formula, Set Formula))

-- | Find two clauses which match, in that one has a literal
-- whose complement is contained in the other literal.
matches :: Set (Set Formula) -> [Match]
matches clauses = do
  let clauseList = Set.toList clauses
  clause <- clauseList
  clause' <- clauseList
  x <- Set.toList clause
  let x' = complement x
  guard $ Set.member x' clause'
  return ((x, clause), (x', clause'))

-- | Apply the resolution rule and return the result
-- if it is not already in the set of axioms.
resolve :: Set (Set Formula) -> [Match] -> Maybe (Set Formula)
resolve clauses matchings
  | [] <- ms = Nothing
  | (m:_) <- ms = Just $ merge m
  where ms = dropWhile ignored matchings
        merge ((v, c), (v', c')) = Set.delete v $ Set.delete v' $ c `Set.union` c'
        ignored m = let c = merge m
                    in c `Set.member` clauses || isTautology c

-- | Whether the given clause is a trivial tautology
-- due to having a literal and its complement in the disjunction.
isTautology :: Set Formula -> Bool
isTautology s = any (\x -> complement x `Set.member` s) s

-- | Returns the complement of the given formula.
-- Ensures that literals remain literals.
complement :: Formula -> Formula
complement (Complement x) = x
complement x              = Complement x
