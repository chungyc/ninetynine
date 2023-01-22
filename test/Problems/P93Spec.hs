{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P93Spec (spec) where

import           Data.Ratio
import qualified Problems.P93          as Problem
import qualified Solutions.P93         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Integer] -> [String]) -> String -> Spec
properties arithmeticPuzzle name = describe name $ do
  prop "has no solution for empty list" $
    arithmeticPuzzle [] `shouldBe` []

  prop "has no solution for singleton list" $ \n ->
    arithmeticPuzzle [n] `shouldBe` []

  prop "are valid equations" $
    forAll numberLists $ \xs ->
    classify (null $ arithmeticPuzzle xs) "trivial" $
    conjoin $ do
      s <- arithmeticPuzzle xs
      return $ s `shouldSatisfy` evalEquation . parseExpr

  prop "includes given numbers in order" $
    forAll numberLists $ \xs ->
    classify (null $ arithmeticPuzzle xs) "trivial" $
    conjoin $ do
      s <- arithmeticPuzzle xs
      return $ s `shouldSatisfy` (==) xs . extractNumbers

  where numberLists = resize 5 $ listOf1 numbers `suchThat` (\l -> length l > 2)
        numbers = chooseInteger (1, 10)

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

-- | Extract the numbers from the strings containing equations.
extractNumbers :: String -> [Integer]
extractNumbers s = extract $ tokenize s
  where extract []           = []
        extract ((N n) : ps) = n : extract ps
        extract (_:ps)       = extract ps

-- | An equation in parsed form.
data Expr = Number Integer
          | Add Expr Expr
          | Subtract Expr Expr
          | Multiply Expr Expr
          | Divide Expr Expr
          | Equals Expr Expr
  deriving Show

-- | Parse an equation from a string.
parseExpr :: String -> Expr
parseExpr s
  | ([], [Ex e]) <- parseExpr' (tokenize s, []) = e
  | otherwise = error $ "parse error: " ++ s

-- | Elements on the stack for parsing the context-free grammar.
data ParseElement = T Token | Ex Expr deriving Show

-- | Manually parse the context-free grammar.
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

-- | Lexical tokens.
data Token = N Integer | P | S | M | D | E | PS | PE
  deriving (Eq, Show)

-- | Parse a string into its lexical tokens.
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

-- | Parse a string into a number and the rest of the string.
parseNumber :: String -> (Integer, String)
parseNumber s = (n, s')
  where n = read $ takeWhile (`elem` ['0'..'9']) s
        s' = dropWhile (`elem` ['0'..'9']) s

evalEquation :: Expr -> Bool
evalEquation (Equals x y) = evalExpr x == evalExpr y
evalEquation e            = error $ "invalid equation: " ++ show e

evalExpr :: Expr -> Ratio Integer
evalExpr (Number x)     = fromIntegral x
evalExpr (Add x y)      = evalExpr x + evalExpr y
evalExpr (Subtract x y) = evalExpr x - evalExpr y
evalExpr (Multiply x y) = evalExpr x * evalExpr y
evalExpr (Divide x y)   = evalExpr x / evalExpr y
evalExpr e              = error $ "invalid expression: " ++ show e
