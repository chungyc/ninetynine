{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P93Spec (spec) where

import           Control.Monad
import           Data.Ratio
import qualified Problems.P93          as Problem
import qualified Solutions.P93         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Integer] -> [String]) -> String -> Spec
properties arithmeticPuzzle name = describe name $ do
  it "has no solution for empty list" $ do
    arithmeticPuzzle [] `shouldBe` []

  prop "has no solution for singleton list" $
    \n -> arithmeticPuzzle [n] `shouldBe` []

  prop "are valid equations" $
    forAll chooseList $ \xs ->
    classify (null $ arithmeticPuzzle xs) "trivial" $
    forM_ (arithmeticPuzzle xs) $ flip shouldSatisfy (evalEquation . parseExpr)

  prop "includes given numbers in order" $
    forAll chooseList $ \xs ->
    classify (null $ arithmeticPuzzle xs) "trivial" $
    arithmeticPuzzle xs `shouldSatisfy` all ((==) xs . extractNumbers)

  where chooseList = resize 6 $ liftM2 (:) chooseNumber $ listOf1 chooseNumber
        chooseNumber = chooseInteger (1, 15)

examples :: Spec
examples = describe "Examples" $ do
  it "arithmeticPuzzle [2,3,5,7,11]" $ do
    arithmeticPuzzle [2,3,5,7,11] `shouldMatchList`
      [ "2 = 3-(5+7-11)"
      , "2 = 3-5-(7-11)"
      , "2 = 3-(5+7)+11"
      , "2 = 3-5-7+11"
      , "2 = (3*5+7)/11"
      , "2*(3-5) = 7-11"
      , "2-(3-(5+7)) = 11"
      , "2-(3-5-7) = 11"
      , "2-(3-5)+7 = 11"
      , "2-3+5+7 = 11"
      ]

  where arithmeticPuzzle = Problem.arithmeticPuzzle

spec :: Spec
spec = parallel $ do
  properties Problem.arithmeticPuzzle "arithmeticPuzzle"
  examples
  describe "From solutions" $ do
    properties Solution.arithmeticPuzzle "arithmeticPuzzle"

data Expr = Number Integer
          | Add Expr Expr
          | Subtract Expr Expr
          | Multiply Expr Expr
          | Divide Expr Expr
          | Equals Expr Expr
  deriving (Eq, Show)

extractNumbers :: String -> [Integer]
extractNumbers s = extract $ tokenize s
  where extract []           = []
        extract ((N n) : ps) = n : extract ps
        extract (_:ps)       = extract ps

parseExpr :: String -> Expr
parseExpr s = e
  where ([], [Ex e]) = parseExpr' (tokenize s, [])

data Token = N Integer | P | S | M | D | E | PS | PE
  deriving (Eq, Show)

data ParseElement = T Token | Ex Expr
  deriving (Eq, Show)

parseExpr' :: ([Token], [ParseElement]) -> ([Token], [ParseElement])
parseExpr' (ts, T (N n) : ps)                 = parseExpr' (ts, Ex (Number n) : ps)
parseExpr' (M:ts, ps@(Ex _ : T P : Ex _ : _)) = parseExpr' (ts, T M : ps)
parseExpr' (D:ts, ps@(Ex _ : T P : Ex _ : _)) = parseExpr' (ts, T D : ps)
parseExpr' (ts, Ex v : T P : Ex u : ps)       = parseExpr' (ts, Ex (Add u v) : ps)
parseExpr' (M:ts, ps@(Ex _ : T S : Ex _ : _)) = parseExpr' (ts, T M : ps)
parseExpr' (D:ts, ps@(Ex _ : T S : Ex _ : _)) = parseExpr' (ts, T D : ps)
parseExpr' (ts, Ex v : T S : Ex u : ps)       = parseExpr' (ts, Ex (Subtract u v) : ps)
parseExpr' (ts, Ex v : T M : Ex u : ps)       = parseExpr' (ts, Ex (Multiply u v) : ps)
parseExpr' (ts, Ex v : T D : Ex u : ps)       = parseExpr' (ts, Ex (Divide u v) : ps)
parseExpr' (ts, T PE : Ex e : T PS : ps)      = parseExpr' (ts, Ex e : ps)
parseExpr' (t:ts, ps)                         = parseExpr' (ts, T t : ps)
parseExpr' ([], [Ex v, T E, Ex u])            = ([], [Ex (Equals u v)])
parseExpr' _                                  = undefined

tokenize :: String -> [Token]
tokenize s = reverse $ snd $ tokenize' (s, [])

tokenize' :: (String, [Token]) -> (String, [Token])
tokenize' ("",ts)     = ("", ts)
tokenize' (' ':xs,ts) = tokenize' (xs, ts)
tokenize' ('+':xs,ts) = tokenize' (xs, P:ts)
tokenize' ('-':xs,ts) = tokenize' (xs, S:ts)
tokenize' ('*':xs,ts) = tokenize' (xs, M:ts)
tokenize' ('/':xs,ts) = tokenize' (xs, D:ts)
tokenize' ('=':xs,ts) = tokenize' (xs, E:ts)
tokenize' ('(':xs,ts) = tokenize' (xs, PS:ts)
tokenize' (')':xs,ts) = tokenize' (xs, PE:ts)
tokenize' (xs,ts)     = tokenize' (xs', N n : ts)
  where (n, xs') = parseNumber xs

parseNumber :: String -> (Integer, String)
parseNumber s = (n, s')
  where n = read $ takeWhile (flip elem ['0'..'9']) s
        s' = dropWhile (flip elem ['0'..'9']) s

evalEquation :: Expr -> Bool
evalEquation (Equals x y) = evalExpr x == evalExpr y
evalEquation _            = undefined

evalExpr :: Expr -> Ratio Integer
evalExpr (Number x)     = fromIntegral x
evalExpr (Add x y)      = evalExpr x + evalExpr y
evalExpr (Subtract x y) = evalExpr x - evalExpr y
evalExpr (Multiply x y) = evalExpr x * evalExpr y
evalExpr (Divide x y)   = evalExpr x / evalExpr y
evalExpr _              = undefined
