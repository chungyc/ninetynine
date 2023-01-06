{- |
Description: Resolution rule
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P53".
-}
module Problems.P53 where

import           Problems.Logic (Formula (..))
import qualified Solutions.P53  as Solution

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

Given a set of axioms and a formula, use the resolution rule to determine
whether the formula is valid given the axioms.

=== Examples

When \(x \wedge y \wedge z\) is true, then obviously \(x \vee y\) is true, so

>>> :{
isTheorem
  [ Variable "X", Variable "Y", Variable "Z" ] $
  Disjoin [ Variable "X", Variable "Y" ]
:}
True

When \(x \to y\) and \(y \to z\), or equivalently \(\neg x \vee y\)
and \(\neg y \vee z\), then it is also the case that \(x \to z\), so

>>> :{
    isTheorem
      [ Disjoin [ Complement $ Variable "X", Variable "Y" ]
      , Disjoin [ Complement $ Variable "Y", Variable "Z" ]
      ] $
      Disjoin [ Complement $ Variable "X", Variable "Z" ]
:}
True

Just because \(x\) is true does not mean that \(y\) is true, so

>>> isTheorem [ Variable "X" ] $ Variable "Y"
False

If there is a contradiction, then everything is both true and false, so

>>> isTheorem [ Variable "X", Complement $ Variable "X" ] $ Value False
True
-}
isTheorem :: [Formula] -> Formula -> Bool
isTheorem = Solution.isTheorem
