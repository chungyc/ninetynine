{-|
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P79Spec (spec) where

import           Data.List             (isPrefixOf)
import           Data.Maybe            (fromJust, isNothing)
import           Problems.Monads       (Element (..), Operator (..),
                                        parsePostfix)
import qualified Problems.P79          as Problem
import qualified Solutions.P79         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Element] -> (Maybe Integer, [([Integer],Operator)])) -> String -> Spec
properties calculatePostfix name = describe name $ do
  it "fails calculation on empty expression" $ do
    calculatePostfix [] `shouldBe` (Nothing, [])

  prop "trivially calculates single number" $ withMaxSuccess 10 $
    \n -> calculatePostfix [Operand n] `shouldBe` (Just n, [])

  prop "not valid with more than one number and no operators" $
    \n -> forAll (listOf1 chooseAny) $ \l ->
      calculatePostfix (Operand n : map Operand l) `shouldSatisfy` isNothing . fst

  context "with individual operators" $ modifyMaxSuccess (const 10) $ do
    prop "calculates expected result for negation" $
      \n -> calculatePostfix [Operand n, Operator Negate] `shouldBe` (Just (-n), [([n], Negate)])

    prop "calculates expected result for addition" $
      \m -> \n ->
        calculatePostfix [Operand m, Operand n, Operator Add]
        `shouldBe` (Just (m+n), [([n,m], Add)])

    prop "calculates expected result for subtraction" $
      \m -> \n ->
        calculatePostfix [Operand m, Operand n, Operator Subtract]
        `shouldBe` (Just (m-n), [([n,m], Subtract)])

    prop "calculates expected result for multiplication" $
      \m -> \n ->
        calculatePostfix [Operand m, Operand n, Operator Multiply]
        `shouldBe` (Just (m*n), [([n,m], Multiply)])

    prop "calculates expected result for division" $
      \m -> \n -> n /= 0 ==>
      calculatePostfix [Operand m, Operand n, Operator Divide]
      `shouldBe` (Just (m `div` n), [([n,m], Divide)])

    prop "calculates expected result for modulo" $
      \m -> \n -> n /= 0 ==>
      calculatePostfix [Operand m, Operand n, Operator Modulo]
      `shouldBe` (Just (m `mod` n), [([n,m], Modulo)])

    prop "fails calculation when dividing by zero" $
      \n -> calculatePostfix [Operand n, Operand 0, Operator Divide] `shouldSatisfy` isNothing . fst

    prop "fails calculation with modulo by zero" $
      \n -> calculatePostfix [Operand n, Operand 0, Operator Modulo] `shouldSatisfy` isNothing . fst

  context "with random lists" $ do
    prop "cannot have history longer than expression" $
      forAll genList $ \e ->
      calculatePostfix e `shouldSatisfy` (<= length e) . length . snd

    prop "has operators appear in same order in history as in expression" $
      forAll genList $ \e ->
      calculatePostfix e `shouldSatisfy`
      (\l -> map (Operator . snd) l `isPrefixOf` filter isOperator e) . snd

  context "with valid expressions" $ do
    prop "calculates expected value" $
      \(Complete (n, e)) -> (fromJust . fst . calculatePostfix) e `shouldBe` n

    prop "has same operators in history as in expression" $
      \(Complete (_, e)) ->
        calculatePostfix e `shouldSatisfy`
        (\l -> map (Operator . snd) l == filter isOperator e) . snd

  where isOperator (Operator _) = True
        isOperator _            = False
        genList = listOf $ oneof [ Operator <$> chooseEnum (minBound, maxBound)
                                 , Operand <$> arbitrary
                                 ]

examples :: Spec
examples = do
  describe "Examples" $ do
    it "calculatePostfix [Operand 8, Operand 5, Operator Subtract]" $ do
      (fst . calculatePostfix) [Operand 8, Operand 5, Operator Subtract] `shouldBe` Just 3

    it "calculatePostfix [Operand 8, Operand 6]" $ do
      (fst . calculatePostfix) [Operand 8, Operand 6] `shouldBe` Nothing

    it "calculatePostfix [Operand 8, Operator Negate]" $ do
      (fst . calculatePostfix) [Operand 8, Operator Negate] `shouldBe` Just (-8)

    it "calculatePostfix [Operand 8, Operator Add]" $ do
      (fst . calculatePostfix) [Operand 8, Operator Add] `shouldBe` Nothing

    it "calculatePostfix $ parsePostfix \"8 5 4 10 + - 3 * negate +\"" $ do
      (calculatePostfix $ parsePostfix "8 5 4 10 + - 3 * negate +") `shouldBe`
        (Just 35, [ ([10,4,5,8], Add)
                  , ([14,5,8], Subtract)
                  , ([3,-9,8], Multiply)
                  , ([-27,8], Negate)
                  , ([27,8], Add)
                  ])

    it "calculatePostfix $ parsePostfix \"1 2 * +\"" $ do
      (calculatePostfix $ parsePostfix "1 2 * +") `shouldBe`
        (Nothing, [ ([2,1], Multiply)
                  , ([2], Add)
                  ])

  where calculatePostfix = Problem.calculatePostfix

spec :: Spec
spec = parallel $ do
  properties Problem.calculatePostfix "calculatePostfix"
  examples
  describe "From solutions" $ do
    properties Solution.calculatePostfix "calculatePostfix"

-- Type for generating a valid expression.
newtype Complete = Complete (Integer, [Element]) deriving (Show)

instance Arbitrary Complete where
  arbitrary = Complete <$> (extract <$> reduction `suchThat` validPartial)
    where reduction = reduce <$>
                      arbitrary <*>
                      infiniteListOf (elements [Add, Subtract, Multiply, Divide, Modulo])
          extract (Partial (Just [n], expr)) = (n, expr)
          extract _                          = undefined

reduce :: Partial -> [Operator] -> Partial
reduce e@(Partial (Just [_], _)) _ = e
-- divide by zero
reduce (Partial (Just (0:_), expr)) (Divide:_) = Partial (Nothing, expr)
-- divide by zero
reduce (Partial (Just (0:_), expr)) (Modulo:_) = Partial (Nothing, expr)
reduce (Partial (Just (x:x':xs), expr)) (op:ops) =
  reduce (Partial (Just ((applyBinary x' x op) : xs), expr ++ [Operator op])) ops
reduce _ _ = undefined

-- Type for building up an expression.
newtype Partial = Partial (Maybe [Integer], [Element]) deriving (Show)

instance Arbitrary Partial where
  arbitrary = sized $ expr
    where expr n | n < 2 = oneof [genNumber]
                 | otherwise = oneof [genNumber, genUnary, genBinary]

genNumber :: Gen Partial
genNumber = numPartial <$> arbitrary

numPartial :: Integer -> Partial
numPartial n = Partial (Just [n], [Operand n])

genUnary :: Gen Partial
genUnary = scale (subtract 1) $
           oneof [negatePartial <$> arbitrary, duplicatePartial <$> arbitrary]
           `suchThat` validPartial

negatePartial :: Partial -> Partial
negatePartial (Partial (Just (x:xs), expr)) = Partial (Just (-x:xs), expr ++ [Operator Negate])
negatePartial (Partial (_, expr)) = Partial (Nothing, expr ++ [Operator Negate])

duplicatePartial :: Partial -> Partial
duplicatePartial (Partial (Just (x:xs), expr)) =
  Partial (Just (x:x:xs), expr ++ [Operator Duplicate])
duplicatePartial (Partial (_, expr)) = Partial (Nothing, expr ++ [Operator Duplicate])

genBinary :: Gen Partial
genBinary =
  scale (`div` 2) $
  oneof [ binaryPartial <$> arbitrary <*> arbitrary <*> elements [Add, Subtract, Multiply]
        , dividePartial <$> arbitrary <*> arbitrary <*> elements [Divide, Modulo]
        -- generate expressions with the Duplicate operator
        , binaryPartial <$> arbitrary <*> empty <*> elements [Add, Subtract, Multiply]
        , dividePartial <$> arbitrary <*> empty <*> elements [Divide, Modulo]
        ]
  `suchThat` validPartial
  where empty = elements [emptyPartial]

binaryPartial :: Partial -> Partial -> Operator -> Partial
binaryPartial (Partial (Just (x:xs), expr)) (Partial (Just [x'], expr')) op =
  Partial (Just ((applyBinary x x' op):xs), expr ++ expr' ++ [Operator op])
binaryPartial (Partial (Just (x:x':xs), expr)) (Partial (Just [], [])) op =
  Partial (Just ((applyBinary x' x op):xs), expr ++ [Operator op])
binaryPartial (Partial (_, expr)) (Partial (_, expr')) op =
  Partial (Nothing, expr ++ expr' ++ [Operator op])

dividePartial :: Partial -> Partial -> Operator -> Partial
-- divide by zero
dividePartial (Partial (_, expr)) (Partial (Just (0:_), expr')) op =
  Partial (Nothing, expr ++ expr' ++ [Operator op])
-- divide by zero
dividePartial (Partial (Just (0:_), expr)) (Partial (Just [], [])) op =
  Partial (Nothing, expr ++ [Operator op])
dividePartial (Partial (Just (x:xs), expr)) (Partial (Just [x'], expr')) op =
  Partial (Just ((applyBinary x x' op):xs), expr ++ expr' ++ [Operator op])
dividePartial (Partial (Just (x:x':xs), expr)) (Partial (Just [], [])) op =
  Partial (Just ((applyBinary x' x op):xs), expr ++ [Operator op])
dividePartial (Partial (_, expr)) (Partial (_, expr')) op =
  Partial (Nothing, expr ++ expr' ++ [Operator op])

applyBinary :: Integer -> Integer -> Operator -> Integer
applyBinary a b Add      = a+b
applyBinary a b Subtract = a-b
applyBinary a b Multiply = a*b
applyBinary a b Divide   = a `div` b
applyBinary a b Modulo   = a `mod` b
applyBinary _ _ _        = undefined

emptyPartial :: Partial
emptyPartial = Partial (Just [], [])

validPartial :: Partial -> Bool
validPartial (Partial (Nothing, _)) = False
validPartial _                      = True
