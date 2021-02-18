{- |
Description: Binary boolean functions

Some solutions to "Problems.P46" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P46 (
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
  -- * Utility values
  functions,
  ) where

import           Problems.P46.Definitions

-- | Truth tables for logical expressions with two operands.
table :: BoolFunc -> [(Bool, Bool, Bool)]
table f = map (\(a, b) -> (a, b, f a b)) [(a, b) | a <- [False, True], b <- [False, True]]

-- | Logical conjunction.
-- I.e., @(and' a b)@ is true if and only if both @a@ and @b@ are true.
and' :: BoolFunc
and' True True = True
and' _ _       = False

-- | Logical disjuncton.
-- I.e., @(or' a b)@ is true if and only if one or both of @a@ and @b@ are true.
or' :: BoolFunc
or' False False = False
or' _ _         = True

-- | Logical alternative denial.
-- I.e., @(nand' a b)@ is true if and only if @(and' a b)@ is false.
nand' :: BoolFunc
nand' True True = False
nand' _ _       = True

-- | Logical joint denial.
-- I.e., @(nor' a b)@ is true if and only if @(or' a b)@ is false.
nor' :: BoolFunc
nor' False False = True
nor' _ _         = False

-- | Logical exclusive disjunction.
-- I.e., @(xor' a b)@ is true if and only if exactly one of @a@ and @b@ is true.
xor' :: BoolFunc
xor' True False = True
xor' False True = True
xor' _ _        = False

-- | Logical implication.
-- I.e., @(impl' a b)@ being true means @b@ must be true if @a@ is true.
-- If @a@ is false, there is no implication as to what @b@ should be.
impl' :: BoolFunc
impl' False False = True
impl' False True  = True
impl' True False  = False
impl' True True   = True

-- | Logical equivalence.
-- I.e., @(equ' a b)@ being true means @a@ is true if and only if @b@ is true.
equ' :: BoolFunc
equ' False False = True
equ' True True   = True
equ' _ _         = False

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
