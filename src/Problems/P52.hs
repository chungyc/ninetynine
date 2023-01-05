{- |
Description: Conjunctive normal form
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P52".
-}
module Problems.P52
  ( toConjunctiveNormalForm
  , Formula (..)
    -- * Supporting functions
    -- | The function below is not part of the problem.
  , evaluateFormula
) where

import           Problems.Logic (Formula (..), evaluateFormula)
import qualified Solutions.P52  as Solution

{- | It is known that any boolean function can be represented in conjunctive normal form.
These are conjunctions of disjunctions of literals,
where literals are one of boolean values, variables, or the complement of values or variables.
For example, the boolean formula \(\neg(x \wedge \neg y) \wedge (z \vee w)\)
is equivalent to the conjunctive normal form \( (\neg x \vee y) \wedge (z \vee w) \).

Return the conjunctive normal form of a boolean formula.
The value returned should always be a conjunction of disjunctions.

=== Examples

>>> toConjunctiveNormalForm $ Value True
Conjoin [Disjoin [Value True]]

>>> toConjunctiveNormalForm $ Complement $ Disjoin [Variable "X", Variable "Y"]
Conjoin [Disjoin [Complement (Variable "X")],Disjoin [Complement (Variable "Y")]]

>>> toConjunctiveNormalForm $ Disjoin [Variable "X", Conjoin [Complement $ Variable "Y", Variable "Z"]]
Conjoin [Disjoin [Variable "X",Complement (Variable "Y")],Disjoin [Variable "X", Variable "Z"]]

=== __Hint__

Transform a boolean formula using De Morgan's law, the distributive law,
and double negation elimination to reduce nesting.
Alternatively, the conjunctive normal form can be obtained
easily from the truth table, although this will always require
a running time exponential to the number of variables.
-}
toConjunctiveNormalForm :: Formula -> Formula
toConjunctiveNormalForm = Solution.toConjunctiveNormalForm
