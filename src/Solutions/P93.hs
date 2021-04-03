{- |
Description: An arithmetic puzzle
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P93" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P93 (arithmeticPuzzle) where

import           Data.List            (inits, nub, tails)
import           Data.Map.Lazy        (Map, (!))
import qualified Data.Map.Lazy        as Map
import           Data.Maybe           (mapMaybe)
import           Data.Ratio
import qualified Data.Set             as Set
import           Problems.BinaryTrees

{- |
Given a list of integer numbers, find a correct way of inserting
the arithmetic signs such that the result is a correct equation.
For example, with the list of numbers @[2,3,5,7,11]@,
we can form the equations @2-3+5+7=11@ or @2=(3*5+7)/11@.

The arithmetic signs to insert are:

* @+@ : addition
* @-@ : subtraction
* @*@ : multiplication
* @/@ : division
* @=@ : equality
* @(@, @)@ : parentheses

Arithmetic operations are only binary, e.g., @-4@ should not be included.
Division should be interpreted as operating on rationals,
e.g., \(3/5 = 6/10\) but \(3/5 \neq 0\), and division by zero should be avoided.
Parentheses should be inserted only when the default precedence rules need to be overridden.
Equality should be inserted exactly once.
-}
arithmeticPuzzle :: [Integer] -> [String]
arithmeticPuzzle []  = []
arithmeticPuzzle [_] = []
arithmeticPuzzle xs  = nub $ map formatEquation es
  where ts = toTrees xs
        es = concat $ map findEquations ts

-- | Form all binary trees that can be formed from the list of integers,
-- where all internal nodes are 'Nothing', which is the placeholder for arithmetic operations,
-- and all leaves are numbers.  All internal nodes have two non-empty subtrees.
toTrees :: [Integer] -> [Tree (Maybe Integer)]
toTrees []  = [Empty]
toTrees [x] = [Branch (Just x) Empty Empty]
toTrees xs  = concat $ map (\(ls,rs) -> [Branch Nothing l r | l <- toTrees ls, r <- toTrees rs]) splits
  where splits = filter (\(ls,rs) -> length ls > 0 && length rs > 0) $ zip (inits xs) (tails xs)

-- | Arithmetic operations which are internal node values for an expression tree.
data Op = Add | Subtract | Multiply | Divide | Equals

-- | Find all equations that can be formed from the binary tree structure.
-- Both sides must be equal to each other.
findEquations :: Tree (Maybe Integer) -> [Tree (Either Op Integer)]
findEquations (Branch Nothing l r) = concat $ map toEquations common
  where ls = findExpressions l
        rs = findExpressions r
        common = Set.toList $ Set.intersection (Map.keysSet ls) (Map.keysSet rs)
        toEquations n = [Branch (Left Equals) lt rt | lt <- ls ! n, rt <- rs ! n]
findEquations _ = undefined  -- need two sides for an equation

-- | Find all expression trees that can be formed from the given binary tree structure.
-- Return a map from their values to the trees.
findExpressions :: Tree (Maybe Integer) -> Map (Ratio Integer) [Tree (Either Op Integer)]
findExpressions t = Map.fromListWith (++) $ mapMaybe (\t' -> assoc $ (evalTree t', t')) $ permuteTree t
  where assoc (Nothing, _) = Nothing
        assoc (Just x, t') = Just (x, [t'])

-- | Compute the value of an expression tree.
-- Returns 'Nothing' if it is invalid, e.g., there would be division by zero.
evalTree :: Tree (Either Op Integer) -> Maybe (Ratio Integer)
evalTree (Branch (Right n) Empty Empty) = Just $ fromIntegral n

evalTree (Branch (Left Divide) l r) = do
  l' <- evalTree l
  r' <- evalTree r
  case r' of 0 -> Nothing
             _ -> return $ evaluate Divide l' r'

evalTree (Branch (Left op) l r) = do
  l' <- evalTree l
  r' <- evalTree r
  return $ evaluate op l' r'

evalTree _                              = undefined

evaluate :: Op -> Ratio Integer -> Ratio Integer -> Ratio Integer
evaluate Add x y      = x + y
evaluate Subtract x y = x - y
evaluate Multiply x y = x * y
evaluate Divide x y   = x / y
evaluate _ _ _        = undefined

permuteTree :: Tree (Maybe Integer) -> [Tree (Either Op Integer)]
permuteTree (Branch Nothing l r)          = [Branch (Left op) l' r' | op <- ops, l' <- ls, r' <- rs]
  where ops = [Add, Subtract, Multiply, Divide]
        ls = permuteTree l
        rs = permuteTree r
permuteTree (Branch (Just n) Empty Empty) = [Branch (Right n) Empty Empty]
permuteTree _                             = undefined  -- should not be possible

-- | Returns the given expression tree in string form.
-- It only inserts parentheses which are necessary.
formatEquation :: Tree (Either Op Integer) -> String
formatEquation (Branch (Right n) Empty Empty) = show n
formatEquation (Branch (Left Equals) l r) = formatEquation l ++ " = " ++ formatEquation r
formatEquation (Branch (Left Add) l r) = formatEquation l ++ "+" ++ formatEquation r

formatEquation (Branch (Left Subtract) l r)
  | isAdd r || isSubtract r = formatEquation l ++ "-(" ++ formatEquation r ++ ")"
  | otherwise               = formatEquation l ++ "-" ++ formatEquation r

formatEquation (Branch (Left Multiply) l r) = format l ++ "*" ++ format r
  where format t | isAdd t || isSubtract t = "(" ++ formatEquation t ++ ")"
                 | otherwise               = formatEquation t

formatEquation (Branch (Left Divide) l r)
  | isMultiply r || isDivide r = format l ++ "/(" ++ formatEquation r ++ ")"
  | otherwise                  = format l ++ "/" ++ format r
  where format t | isAdd t || isSubtract t = "(" ++ formatEquation t ++ ")"
                 | otherwise               = formatEquation t

formatEquation _ = undefined  -- should not be possible

isAdd :: Tree (Either Op Integer) -> Bool
isAdd (Branch (Left Add) _ _) = True
isAdd _                       = False

isSubtract :: Tree (Either Op Integer) -> Bool
isSubtract (Branch (Left Subtract) _ _) = True
isSubtract _                            = False

isMultiply :: Tree (Either Op Integer) -> Bool
isMultiply (Branch (Left Multiply) _ _) = True
isMultiply _                            = False

isDivide ::  Tree (Either Op Integer) -> Bool
isDivide (Branch (Left Divide) _ _) = True
isDivide _                          = False
