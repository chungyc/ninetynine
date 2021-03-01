-- | Supporting definitions for logic and code problems.
module Problems.Logic (
  BoolFunc,
  -- * Utility functions
  printTable,
  printTablen,
  Functions (Functions, getTable, getAnd, getOr, getNand, getNor, getXor, getImpl, getEqu),
  ) where

import           Data.List (intercalate, sort)

-- | Define boolean functions 'Problems.P46.and'', 'Problems.P46.or'', 'Problems.P46.nand'',
-- 'Problems.P46.nor'', 'Problems.P46.xor'', 'Problems.P46.impl'', and 'Problems.P46.equ'',
-- which succeed or fail according to the result of their respective operations;
-- e.g., @(and' a b)@ is true if and only if both @a@ and @b@ are true.
-- Do not use the builtin boolean operators.
--
-- A logical expression in two variables can then be written as in the following example:
--
-- > \a -> \b -> and' (or' a b) (nand' a b)
--
-- Write a function 'Problems.P46.table' which returns
-- the truth table of a given logical expression in two variables.
--
-- === __Notes__
--
-- There is no technical reason to define the type synonym 'BoolFunc'.
-- However, it is a good place to explain the entire problem
-- without talking about writing truth table functions first,
-- especially for inclusion in the documentation for "Problems".
--
-- The [original problem for Haskell](https://wiki.haskell.org/99_questions/46_to_50)
-- required that the truth table be printed:
-- /"Now, write a predicate table\/3 which prints the truth table of a given logical expression in two variables."/
-- It was changed here to return a list of boolean tuples, because the requirement
-- for I/O seemed uninteresting in the context of the rest of the problem.
--
-- Documentation for the expected semantics for each boolean function was added,
-- and the example for 'Problems.P46.table' was modified to avoid sensitivity to order.
type BoolFunc = Bool -> Bool -> Bool

-- | Print truth table as returned by 'Problems.P46.table'.
--
-- Given the same pair of truth tables except for order, the output will be the same.
printTable :: [(Bool, Bool, Bool)] -> IO ()
printTable ts = mapM_ (putStrLn . showRow) $ sort ts
  where showRow (a, b, c) = showBool a ++ " " ++ showBool b ++ " " ++ showBool c
        showBool a = if a then "True " else "False"

-- | Print truth table as returned by 'Problems.P48.tablen'.
--
-- Given the same pair of truth tables except for order, the output will be the same.
printTablen :: [[Bool]] -> IO ()
printTablen t = mapM_ (putStrLn . showRow) $ sort t
  where showRow xs = intercalate " " $ map showBool xs
        showBool a = if a then "True " else "False"

-- | Set of functions grouped together.
--
-- Useful for passing in a particular set of functions.
-- E.g., testing and benchmarking particular implementations.
data Functions = Functions { getTable :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
                           , getAnd   :: Bool -> Bool -> Bool
                           , getOr    :: Bool -> Bool -> Bool
                           , getNand  :: Bool -> Bool -> Bool
                           , getNor   :: Bool -> Bool -> Bool
                           , getXor   :: Bool -> Bool -> Bool
                           , getImpl  :: Bool -> Bool -> Bool
                           , getEqu   :: Bool -> Bool -> Bool
                           }
