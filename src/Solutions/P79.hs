{- |
Description: Postfix notation
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P79" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P79 (calculatePostfix) where

import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer
import           Problems.PostfixNotation

{- | Postfix notation, also known as reverse Polish notation,
has operators come after their operands in mathematical expressions.
It has no need for operator precedence or parentheses to specify evaluation order.

Evaluation is typically done using a stack.  Numbers are pushed onto the stack,
and operators pop out numbers and pushes back the result.
The t'Control.Monad.State.Lazy.State' monad would be useful for maintaining such a stack.

There may be errors with some expressions.  For example, an expression may be ill-formed,
or there may be a division by zero.  It would be useful to use the 'Maybe' monad so
that we can return 'Nothing' if there is an error.

Finally for this problem, we would like to keep track of
which operators are applied to which numbers.
The function should also return a list of stack and operator pairs,
with each entry showing the state of the stack when the operator is about to be applied.
Logging each entry can be done with the t'Control.Monad.Writer.Lazy.Writer' monad.

Unfortunately, it would be very cumbersome to use these monads directly together.
Monad transformers are a way to make it substantially easier to use more than one monad
at the same time.  Use monad transformers to compose the t'Control.Monad.State.Lazy.State',
'Maybe', and t'Control.Monad.Writer.Lazy.Writer' monads into a single monad to implement
a function which evaluates an expression in postfix notation.  It should also
return the history of the calculation.
-}
calculatePostfix :: [Element] -> (Maybe Integer, [([Integer], Operator)])
calculatePostfix xs = (extract result, calculations)
  where (result, calculations) = run $ calculatePostfix' xs
        run f = runIdentity $ runWriterT $ runStateT (runMaybeT f) []
        extract (Nothing, _)   = Nothing
        extract (Just (), [x]) = Just $ x
        extract _              = Nothing

type Stack = [Integer]

type Calculation = MaybeT (StateT Stack (Writer [(Stack, Operator)]))

calculatePostfix' :: [Element] -> Calculation ()
calculatePostfix' (Operand n : xs) = do
  push n
  calculatePostfix' xs
calculatePostfix' (Operator op : xs) = do
  stack <- get
  tell [(stack, op)]
  calculate op
  calculatePostfix' xs
calculatePostfix' [] = return ()

calculate :: Operator -> Calculation ()
calculate Negate = do
  n <- pop
  push $ -n
calculate Duplicate = do
  n <- peek
  push n
calculate op = do
  b <- pop
  a <- pop
  n <- binaryOp op a b
  push n

binaryOp :: Operator -> Integer -> Integer -> Calculation Integer
binaryOp Add a b      = return $ a+b
binaryOp Subtract a b = return $ a-b
binaryOp Multiply a b = return $ a*b
binaryOp Divide _ 0   = mzero
binaryOp Divide a b   = return $ a `div` b
binaryOp Modulo _ 0   = mzero
binaryOp Modulo a b   = return $ a `mod` b
binaryOp _ _ _        = mzero

push :: Integer -> Calculation ()
push x = modify (x:)

pop :: Calculation Integer
pop = do
  xs <- get
  guard $ not $ null xs
  put $ tail xs
  return $ head xs

peek :: Calculation Integer
peek = do
  xs <- get
  guard $ not $ null xs
  return $ head xs
