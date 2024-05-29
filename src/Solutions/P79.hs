{- |
Description: Postfix notation
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P79" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P79 (calculatePostfix) where

import           Control.Monad (guard, mzero)
import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer
import           Problems.Monads           (Element (..), Operator (..))

{- | Use monad transformers to evaluate postfix notation.

Returns @Nothing@ when there is an error.  Also returns the history of the evaluation.
-}
calculatePostfix :: [Element] -> (Maybe Integer, [([Integer], Maybe Operator)])
calculatePostfix xs = (extract result, calculations)
  where (result, calculations) = run $ calculatePostfix' xs
        run f = runIdentity $ runWriterT $ runStateT (runMaybeT f) []
        extract (Nothing, _)   = Nothing
        extract (Just (), [x]) = Just x
        extract _              = Nothing

type Stack = [Integer]

type Calculation = MaybeT (StateT Stack (Writer [(Stack, Maybe Operator)]))

calculatePostfix' :: [Element] -> Calculation ()
calculatePostfix' (Operand n : xs) = do
  push n
  stack <- get
  tell [(stack, Nothing)]
  calculatePostfix' xs
calculatePostfix' (Operator op : xs) = do
  calculate op
  stack <- get
  tell [(stack, Just op)]
  calculatePostfix' xs
calculatePostfix' [] = return ()

calculate :: Operator -> Calculation ()
calculate Negate = do
  n <- pop
  push $ -n
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
