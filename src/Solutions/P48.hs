{- |
Description: `tablen`

Some solutions to "Problems.P48" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P48 (tablen) where

-- | Truth tables for logical expressions with an arbitrary number of variables.
--
-- Generalize "Problems.P46" in a way that the logical expression may contain any number of logical variables.
-- Define 'tablen' such that @tablen n f@ prints the truth table for the expression @f@,
-- which contains @n@ logical variables.
tablen :: Int -> ([Bool] -> Bool) -> [[Bool]]
tablen n f = map (\xs -> xs ++ [f xs]) $ enumerate n

-- | Enumerate all combinations of n boolean values.
enumerate :: Int -> [[Bool]]
enumerate 0 = [[]]
enumerate n = [x : xs | x <- [False, True], xs <- enumerate (n-1)]
