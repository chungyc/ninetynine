{- |
Description: Supporting definitions for postfix notation
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Supporting definitions for using postfix notation.
In particular, this supports "Problems.P79".
-}
module Problems.PostfixNotation (Operator (..), Element (..), parsePostfix) where

import           Data.Char (isDigit)

-- | Encodes an operator for a mathematical expression.
data Operator
  -- | Encodes negation.  Equivalent to an unary minus.  Unary operator.
  = Negate
  -- | Encodes duplication.  Makes another copy of its operand.  Unary operator.
  | Duplicate
  -- | Encodes addition.  Binary operator.
  | Add
  -- | Encodes subtraction.  Binary operator.
  | Subtract
  -- | Encodes multiplication.  Binary operator.
  | Multiply
  -- | Encodes division.  Equivalent to 'div'.  Binary operator.
  | Divide
  -- | Encodes a modulo operator.  Equivalent to 'mod'.  Binary operator.
  | Modulo
  deriving (Eq, Enum, Bounded, Show)

-- | A single element within a mathematical expression.
-- A list of these elements comprises an expression in postfix notation.
data Element = Operator Operator | Operand Integer deriving (Eq, Show)

-- | Parses a string containing a mathematical expression in postfix notation.
-- This can make it easier to write down an expression in a more conventional form.
--
-- For example,
--
-- >>> parsePostfix "3 4 2 - *"
-- [Operand 3,Operand 4,Operand 2,Operator Subtract,Operator Multiply]
--
-- The operators are encoded as follows:
--
-- +-------------+------------------+
-- | Operator    | String           |
-- +=============+==================+
-- | 'Negate'    | @"negate"@       |
-- +-------------+------------------+
-- | 'Duplicate' | @"duplicate"@    |
-- +-------------+------------------+
-- | 'Add'       | @"+"@            |
-- +-------------+------------------+
-- | 'Subtract'  | @"-"@            |
-- +-------------+------------------+
-- | 'Multiply'  | @"*"@            |
-- +-------------+------------------+
-- | 'Divide'    | @"/"@            |
-- +-------------+------------------+
-- | 'Modulo'    | @"%"@            |
-- +-------------+------------------+
parsePostfix :: String -> [Element]
parsePostfix expr = map parseToken $ words expr

parseToken :: String -> Element
parseToken x
  | all isDigit x = Operand (read x)
  | x == "negate" = Operator Negate
  | x == "duplicate" = Operator Duplicate
  | x == "+" = Operator Add
  | x == "-" = Operator Subtract
  | x == "*" = Operator Multiply
  | x == "/" = Operator Divide
  | x == "%" = Operator Modulo
  | otherwise = undefined
