{- |
Description: Truth tables for logical expressions
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P46".
-}
module Problems.P46 (
  BoolFunc,
  table,
  -- * Boolean functions
  and',
  or',
  nand',
  nor',
  xor',
  impl',
  equ',
  -- * Utility functions
  functions,
  ) where

import           Problems.Logic
import qualified Solutions.P46  as Solution

-- | Truth tables for logical expressions with two operands.
--
-- The truth table will be a list of @('Bool', 'Bool', 'Bool')@ tuples.
-- If @f@ is the logical expression, a tuple @(a, b, c)@ in the list
-- will mean that @(f a b == c)@.  The list will include all possible
-- distinct combinations of the tuples.
--
-- === Examples
--
-- >>> printTable $ table (\a b -> (and' a (or' a b)))
-- False False False
-- False True  False
-- True  False True
-- True  True  True
table :: BoolFunc -> [(Bool, Bool, Bool)]
table = Solution.table

-- | Logical conjunction.
-- I.e., @(and' a b)@ is true if and only if both @a@ and @b@ are true.
--
-- === Examples
--
-- >>> and' True False
-- False
and' :: BoolFunc
and' = Solution.and'

-- | Logical disjuncton.
-- I.e., @(or' a b)@ is true if and only if one or both of @a@ and @b@ are true.
--
-- === Examples
--
-- >>> or' True False
-- True
or' :: BoolFunc
or' = Solution.or'

-- | Logical alternative denial.
-- I.e., @(nand' a b)@ is true if and only if @(and' a b)@ is false.
--
-- === Examples
--
-- >>> nand' True False
-- True
nand' :: BoolFunc
nand' = Solution.nand'

-- | Logical joint denial.
-- I.e., @(nor' a b)@ is true if and only if @(or' a b)@ is false.
--
-- === Examples
--
-- >>> nor' True False
-- False
nor' :: BoolFunc
nor' = Solution.nor'

-- | Logical exclusive disjunction.
-- I.e., @(xor' a b)@ is true if and only if exactly one of @a@ and @b@ is true.
--
-- === Examples
--
-- >>> xor' True False
-- True
xor' :: BoolFunc
xor' = Solution.xor'

-- | Logical implication.
-- I.e., @(impl' a b)@ being true means @b@ must be true if @a@ is true.
-- If @a@ is false, there is no implication as to what @b@ should be.
--
-- === Examples
--
-- >>> impl' True False
-- False
impl' :: BoolFunc
impl' = Solution.impl'

-- | Logical equivalence.
-- I.e., @(equ' a b)@ being true means @a@ is true if and only if @b@ is true.
--
-- === Examples
--
-- >>> equ' True False
-- False
equ' :: BoolFunc
equ' = Solution.equ'

-- | Functions in this module grouped together.
--
-- They are grouped together for testing or benchmarking purposes.
functions :: Functions
functions = Functions { getTable = table
                      , getAnd = and'
                      , getOr = or'
                      , getNand = nand'
                      , getNor = nor'
                      , getXor = xor'
                      , getImpl = impl'
                      , getEqu = equ' }
