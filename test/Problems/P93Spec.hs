{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P93Spec (spec) where

import           Data.Maybe            (catMaybes)
import           Data.Ratio
import qualified Problems.P93          as Problem
import qualified Solutions.P93         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language  (emptyDef)
import qualified Text.Parsec.Token     as Token

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
        numbers = chooseInteger (1, 12)

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
extractNumbers s | Right l <- parse numbers "" s = catMaybes l
                 | otherwise = undefined
  where numbers = many $ (Just <$> natural) <|> (anyChar >> return Nothing)
        natural = Token.natural $ Token.makeTokenParser emptyDef

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
parseExpr s | Right e <- p = e
            | Left err <- p = error $ show err

  where p = parse expr "" s

        expr = buildExpressionParser table term
        term = parens expr <|> number
        number = Number <$> natural

        table = [ [ Infix (op "*" >> return Multiply) AssocLeft
                  , Infix (op "/" >> return Divide) AssocLeft
                  ]
                , [ Infix (op "+" >> return Add) AssocLeft
                  , Infix (op "-" >> return Subtract) AssocLeft
                  ]
                , [ Infix (op "=" >> return Equals) AssocNone ]
                ]

        parens = Token.parens lexer
        op = Token.reservedOp lexer
        natural = Token.natural lexer
        lexer = Token.makeTokenParser emptyDef

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
