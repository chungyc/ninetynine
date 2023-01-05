{- |
Description: Conjunctive normal form
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P52" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P52 (toConjunctiveNormalForm) where

import           Data.List      (partition)
import           Problems.Logic (Formula (..))

{- | It is known that any boolean function can be represented in conjunctive normal form.
These are conjunctions of disjunctions of literals,
where literals are one of boolean values, variables, or the complement of values or variables.
For example, the boolean formula \(\neg(x \wedge \neg y) \wedge (z \vee w)\)
is equivalent to the conjunctive normal form \( (\neg x \vee y) \wedge (z \vee w) \).

Return the conjunctive normal form of a boolean formula.
The value returned should always be a conjunction of disjunctions.
-}
toConjunctiveNormalForm :: Formula -> Formula
toConjunctiveNormalForm f
  | isLiteral f' = Conjoin [ Disjoin [f'] ]
  | Conjoin fs <- f' = Conjoin $ map toDisjoin fs
  | Disjoin fs <- f', all isLiteral fs = Conjoin [f']
  | Disjoin _ <- f' = explodeDisjunction f'
  | otherwise = error $ "unexpected form: " ++ show f'
  where f' = flatten $ normalizeFormulaOrder $ normalizeComplement $ normalizeEmptyClauses f

-- Normalize a formula so that it has no conjunction or disjunction
-- has an empty set of clauses.  This allows us to avoid having
-- to treat such conjunctions or disjunctions specially.
normalizeEmptyClauses :: Formula -> Formula
normalizeEmptyClauses f@(Value _)    = f
normalizeEmptyClauses f@(Variable _) = f
normalizeEmptyClauses (Complement f) = Complement $ normalizeEmptyClauses f
normalizeEmptyClauses (Disjoin [])   = Value False
normalizeEmptyClauses (Conjoin [])   = Value True
normalizeEmptyClauses (Disjoin fs)   = Disjoin $ map normalizeEmptyClauses fs
normalizeEmptyClauses (Conjoin fs)   = Conjoin $ map normalizeEmptyClauses fs

-- Ensure that complement only applies to values or variables.
-- I.e., return value will only have complement in literals.
normalizeComplement :: Formula -> Formula
normalizeComplement f@(Value _) = f
normalizeComplement f@(Variable _) = f
normalizeComplement (Complement (Value v)) = Value $ not v
normalizeComplement f@(Complement (Variable _)) = f
normalizeComplement (Complement (Complement f)) = normalizeComplement f
normalizeComplement (Disjoin fs) = Disjoin $ map normalizeComplement fs
normalizeComplement (Conjoin fs) = Conjoin $ map normalizeComplement fs
normalizeComplement (Complement (Disjoin fs)) =
  Conjoin $ map (normalizeComplement . Complement) fs
normalizeComplement (Complement (Conjoin fs)) =
  Disjoin $ map (normalizeComplement . Complement) fs

-- Normalize the order of clauses in the formula so that:
--
-- * For conjunctions, the clauses are in the order of
--   conjunctions, compound disjunctions, flat disjunctions, and literals.
--
-- * For disjunctions, the clauses are in the order of
--   disjunctions, compound conjunctions, flat disjunctions, and literals.
--
-- This makes it more convenient to merge clauses or apply the distributive law.
normalizeFormulaOrder :: Formula -> Formula
normalizeFormulaOrder f@(Value _)    = f
normalizeFormulaOrder f@(Variable _) = f
normalizeFormulaOrder (Complement f) = Complement $ normalizeFormulaOrder f
normalizeFormulaOrder (Disjoin fs)   = normalizeDisjunction $ Disjoin $ map normalizeFormulaOrder fs
normalizeFormulaOrder (Conjoin fs)   = normalizeConjunction $ Conjoin $ map normalizeFormulaOrder fs

-- Normalize the order of clauses in a disjunction.
-- It does not order the clauses in sub-clauses.
normalizeDisjunction :: Formula -> Formula
normalizeDisjunction (Disjoin fs) =
  Disjoin $ disjunctions ++ compoundConjunctions ++ flatConjunctions ++ literals
  where (disjunctions, remainder) = partition isDisjunction fs
        (conjunctions, literals) = partition isConjunction remainder
        (compoundConjunctions, flatConjunctions) = partition (not . isFlat) conjunctions
normalizeDisjunction f = f

-- Normalize the order of clauses in a conjunction.
-- It does not order the clauses in sub-clauses.
normalizeConjunction :: Formula -> Formula
normalizeConjunction (Conjoin fs) =
  Conjoin $ conjunctions ++ compoundDisjunctions ++ flatDisjunctions ++ literals
  where (conjunctions, remainder) = partition isConjunction fs
        (disjunctions, literals) = partition isDisjunction remainder
        (compoundDisjunctions, flatDisjunctions) = partition (not . isFlat) disjunctions
normalizeConjunction f = f

isLiteral :: Formula -> Bool
isLiteral (Value _)                 = True
isLiteral (Variable _)              = True
isLiteral (Complement (Value _))    = True
isLiteral (Complement (Variable _)) = True
isLiteral _                         = False

isDisjunction :: Formula -> Bool
isDisjunction (Disjoin _) = True
isDisjunction _           = False

isConjunction :: Formula -> Bool
isConjunction (Conjoin _) = True
isConjunction _           = False

isFlat :: Formula -> Bool
isFlat (Conjoin fs) = all isLiteral fs
isFlat (Disjoin fs) = all isLiteral fs
isFlat f | isLiteral f = True
         | otherwise = False

-- Turn the given formula into a disjunction,
-- if it is not already a disjunction.
-- Used for turning a simpler representation to
-- the full conjunctive normal form.
toDisjoin :: Formula -> Formula
toDisjoin f@(Disjoin _) = f
toDisjoin f             = Disjoin [f]

-- Flattens a boolean formula until it is at most two levels deep.
-- At all levels, the formula as it is transformed will be in a normalized form;
-- i.e., no empty set of clauses, complement only applies to values and variables,
-- and clauses are in normalized order.
flatten :: Formula -> Formula
flatten f@(Value _)                 = f
flatten f@(Variable _)              = f
flatten f@(Complement (Value _))    = f
flatten f@(Complement (Variable _)) = f
flatten (Complement (Complement f)) = f
flatten f@(Disjoin _)               = flattenDisjunction f
flatten f@(Conjoin _)               = flattenConjunction f
flatten f                           = error $ "unexpected form: " ++ show f

flattenDisjunction :: Formula -> Formula
flattenDisjunction f@(Disjoin fs)
  | [] <- fs = Value False
  | all isLiteral fs = f
  | [f'] <- fs = flatten f'
  | Disjoin fs' : fs'' <- fs = flattenAgain $ Disjoin $ fs' ++ fs''
  | all isFlat fs = f
  -- Apply the distributive law.  In particular, it raises the disjunction
  -- to a higher level so that it can be merged with this level.
  | Conjoin (Disjoin fs' : fs'') : fs''' <- fs =
      let distributed = Disjoin $ map (\x -> normalizeConjunction $ Conjoin $ x : fs'') fs'
      in flattenAgain $ Disjoin $ distributed : fs'''
  | otherwise = flattenAgain $ Disjoin $ map flatten fs
  where flattenAgain = flatten . normalizeDisjunction
flattenDisjunction f = f

flattenConjunction :: Formula -> Formula
flattenConjunction f@(Conjoin fs)
  | [] <- fs = Value True
  | all isLiteral fs = f
  | [f'] <- fs = flatten f'
  | Conjoin fs' : fs'' <- fs = flattenAgain $ Conjoin $ fs' ++ fs''
  | all isFlat fs = f
  -- Apply the distributive law.  In particular, it raises the conjunction
  -- to a higher level so that it can be merged with this level.
  | Disjoin (Conjoin fs' : fs'') : fs''' <- fs =
      let distributed = Conjoin $ map (\x -> normalizeDisjunction $ Disjoin $ x : fs'') fs'
      in flattenAgain $ Conjoin $ distributed : fs'''
  | otherwise = flattenAgain $ Conjoin $ map flatten fs
  where flattenAgain = flatten . normalizeConjunction
flattenConjunction f = f

-- Combine all the terms in the conjunctions in a disjunction so that
-- the result is an equivalent conjunction of disjunctions.
explodeDisjunction :: Formula -> Formula
explodeDisjunction (Disjoin fs) = Conjoin $ map Disjoin $ sequence $ toList fs
  where toList (Conjoin fs' : fs'') = fs' : toList fs''
        toList (f : fs')            = [f] : toList fs'
        toList []                   = []
explodeDisjunction f = f
