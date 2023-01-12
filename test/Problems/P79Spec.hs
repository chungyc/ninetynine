{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P79Spec where -- (spec) where

import           Data.List             (isPrefixOf)
import           Data.Maybe            (fromJust, isJust, isNothing)
import           Problems.Monads       (Element (..), Operator (..),
                                        parsePostfix)
import qualified Problems.P79          as Problem
import qualified Solutions.P79         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties
  :: ([Element] -> (Maybe Integer, [([Integer], Maybe Operator)]))
  -> String
  -> Spec
properties calculatePostfix name = describe name $ do
  prop "fails calculation on empty expression" $
    calculatePostfix [] `shouldBe` (Nothing, [])

  prop "trivially calculates single number" $
    \n -> calculatePostfix [Operand n]
    `shouldBe` (Just n, [ ([n], Nothing) ])

  prop "is not valid with more than one number and no operators" $
    forAll (listOf1 arbitrary) $ \l -> length l > 1 ==>
    calculatePostfix (map Operand l) `shouldSatisfy` isNothing . fst

  context "with results" $ do
    prop "calculates expected result" $
      \(Calculation { expression = expr, result = x}) ->
        counterexample (show expr) $
        calculatePostfix expr `shouldHaveResult` x

    prop "calculates negation" $
      \(Calculation { expression = expr, result = x}) ->
        counterexample (show $ expr ++ [Operator Negate]) $
        calculatePostfix (expr ++ [Operator Negate]) `shouldHaveResult` -x

    context "with non-division binary operators" $
      let test op f = prop ("calculates " ++ show op) $
            \(Calculation { expression = expr, result = x }) ->
            \(Calculation { expression = expr', result = x' }) ->
              let e = expr ++ expr' ++ [Operator op]
              in counterexample (show e) $
                 calculatePostfix e `shouldHaveResult` f x x'
      in do test Add (+)
            test Subtract (-)
            test Multiply (*)

    context "with division-based binary operators" $
      let test op f = prop ("calculates " ++ show op) $
            \(Calculation { expression = expr, result = x }) ->
            \(Calculation { expression = expr', result = x' }) ->
              x' /= 0 ==>
              let e = expr ++ expr' ++ [Operator op]
              in counterexample (show e) $
                 calculatePostfix e `shouldHaveResult` f x x'
      in do test Divide div
            test Modulo mod

    context "with divisions by zero" $
      let test op = prop ("returns nothing for " ++ show op) $
            \(Calculation { expression = expr }) ->
            \(Calculation { expression = expr', result = x' }) ->
            forAll (expressionsContaining $ expr ++ expr' ++ [Operator op]) $ \e ->
              classify (x' == 0) "expression is zero" $
              disjoin
                [ counterexample (show e) $
                  x' == 0 &&
                  isNothing (fst $ calculatePostfix e)

                  -- If x' is not zero, we can still use e' to test
                  -- division by zero in a much less general way.
                , let e' = expr ++ [ Operand 0, Operator op]
                  in counterexample (show e') $
                     x' /= 0 &&
                     isNothing (fst $ calculatePostfix e')
                ]
      in do test Divide
            test Modulo

  context "with history" $ do
    prop "has history for calculation with no errors" $ \c ->
      counterexample (show $ expression c) $
      counterexample (show $ history c) $
      calculatePostfix (expression c) `shouldSatisfy` (==) (history c) . snd

    prop "cannot have history longer than expression" $
      forAll expressions $ \e ->
      calculatePostfix e `shouldSatisfy` (<= length e) . length . snd

    prop "has operators appear in same order in history as in expression" $
      forAll expressions $ \e ->
      calculatePostfix e `shouldSatisfy`
      (\l -> map (Operator . fromJust . snd) (filter (isJust . snd) l)
             `isPrefixOf` filter isOperator e) . snd

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
        (Just 35, [ ([8], Nothing)
                  , ([5,8], Nothing)
                  , ([4,5,8], Nothing)
                  , ([10,4,5,8], Nothing)
                  , ([14,5,8], Just Add)
                  , ([-9,8], Just Subtract)
                  , ([3,-9,8], Nothing)
                  , ([-27,8], Just Multiply)
                  , ([27,8], Just Negate)
                  , ([35], Just Add)
                  ])

    it "calculatePostfix $ parsePostfix \"1 2 * +\"" $ do
      (calculatePostfix $ parsePostfix "1 2 * +") `shouldBe`
        (Nothing, [ ([1], Nothing)
                  , ([2,1], Nothing)
                  , ([2], Just Multiply)
                  ])

  where calculatePostfix = Problem.calculatePostfix

spec :: Spec
spec = parallel $ do
  properties Problem.calculatePostfix "calculatePostfix"
  examples
  describe "From solutions" $ do
    properties Solution.calculatePostfix "calculatePostfix"

infix 1 `shouldHaveResult`

-- Analog to shouldSatisfy specialized to calculatePostfix.
--
-- The return value from calculatePostfix must have a result,
-- and the result must be what is expected
shouldHaveResult
  :: (Maybe Integer, [([Integer], Maybe Operator)])
  -> Integer
  -> Expectation
shouldHaveResult x expected = x `shouldSatisfy` (==) expected . getResult
  where getResult (Just n, _) = n
        getResult _           = error "no valid result"

-- | Whether the given element is an operator.
isOperator :: Element -> Bool
isOperator (Operator _) = True
isOperator _            = False

-- Whether the operator is a division-based operator, and if so,
-- whether dividing by the given number would be dividing by zero.
isDividesByZero :: Integer -> Operator -> Bool
isDividesByZero 0 Divide = True
isDividesByZero 0 Modulo = True
isDividesByZero _ _      = False

-- | Applies a unary operator to a number.
unaryOp :: Operator -> Integer -> Integer
unaryOp Negate n = -n
unaryOp _ _      = error "not an unary operator"

-- | Applies a binary operator to two numbers.
binaryOp :: Operator -> Integer -> Integer -> Integer
binaryOp Add x y      = x + y
binaryOp Subtract x y = x - y
binaryOp Multiply x y = x * y
binaryOp Divide x y   = x `div` y
binaryOp Modulo x y   = x `mod` y
binaryOp _ _ _        = error "not a binary operator"

-- | A full calculation evaluating postfix notation.
--
-- The full evaluation tree is included instead of just generating
-- an expression to conveniently shrink test cases.
data Calculation = Calculation
  { expression :: [Element]  -- ^ Expression being calculated.
  , element    :: Element  -- ^ Operand or operator responsible for result.
  , result     :: Integer  -- ^ Calculated result.
  , inputs     :: [Calculation]  -- ^ Calculations serving as input to this one.
  }
  deriving Show

instance Arbitrary Calculation where
  arbitrary = calculations
  shrink = shrinkCalculation

-- | Generate a calculation, which will be well-formed and without error.
--
-- We generate expressions through generating a calculation instead
-- of generating expressions directly according to their syntax.
-- The latter has a very low rate of constructing an expression
-- which will have no errors.
calculations :: Gen Calculation
calculations = sized gen
  where gen 0 = num
        gen _ = oneof [ num, unary, binary ]

        num = do
          x <- arbitrary
          let e = Operand x
          return $ Calculation
            { expression = [e]
            , element = e
            , result = x
            , inputs = [] }

        unary = scale (subtract 1) $ do
          c@(Calculation { expression = expr, result = x }) <- calculations
          op <- unaryOperators
          let e = Operator op
          return $ Calculation
            { expression = expr ++ [e]
            , element = e
            , result = unaryOp op x
            , inputs = [c]
            }

        binary = scale (`div` 2) $ do
          c@(Calculation { expression = expr, result = x }) <- calculations
          c'@(Calculation { expression = expr', result = x'}) <- calculations
          op <- binaryOperators `suchThat` (not . isDividesByZero x')
          let e = Operator op
          return $ Calculation
            { expression = expr ++ expr' ++ [e]
            , element = e
            , result = binaryOp op x x'
            , inputs = [ c, c' ]
            }

-- | Shrink a calculation.
shrinkCalculation :: Calculation -> [Calculation]
shrinkCalculation (Calculation { inputs = [] }) = []
shrinkCalculation c@(Calculation { result = x, inputs = es }) =
  [ toOperand x ] ++ shrinkInputs c ++ es

  where
    -- Replace an entire calculation with just a number.
    toOperand y = Calculation { expression = [ Operand y ]
                              , element = Operand y
                              , result = y
                              , inputs = []
                              }

    -- shrinkInputs will shrink the input calculations.
    -- It will keep the results the same.
    shrinkInputs (Calculation { element = Operator op
                              , result = y
                              , inputs = cs
                              }) =
      -- Replace the expression with a single number.
      [ toOperand y ] ++

      -- Shrink each of the inputs that go into this calculation.
      [ Calculation { expression = concatMap expression cs' ++ [ Operator op ]
                    , element = Operator op
                    , result = y
                    , inputs = cs'
                    }
      | cs' <- shrinkInputList cs ]

      where
        shrinkInputList []     = []
        shrinkInputList [z]    = [ [z'] | z' <- shrinkInputs z ]
        shrinkInputList (z:zs) = [ z' : zs | z' <- shrinkInputs z ] ++
                                 [ z : zs' | zs' <- shrinkInputList zs ]

    shrinkInputs _ = []

-- | Generate unary operators.
unaryOperators :: Gen Operator
unaryOperators = pure $ Negate

-- | Generate binary operators.
binaryOperators :: Gen Operator
binaryOperators = elements [ Add, Subtract, Multiply, Divide, Modulo ]

-- | Generate random expressions, regardless of validity.
expressions :: Gen [Element]
expressions = listOf $ oneof [ Operator <$> elements [minBound..maxBound]
                             , Operand <$> arbitrary
                             ]

-- | Generate an expression containing the given expression.
--
-- If the given expression is valid,
-- then the generated expression will also be valid.
expressionsContaining :: [Element] -> Gen [Element]
expressionsContaining e = do
  e' <- expression <$> arbitrary
  (l, _, l') <- elements $ candidates e'
  return $ l ++ e ++ l'

  where
    -- Candidate splits of a generated expression,
    -- where the second element is an operand
    -- which can be replaced by an entire expression.
    --
    -- A complete expression will evaluate to a single operand,
    -- so replacing an operand with a complete expression
    -- will always result in a well-formed expression.
    candidates expr = map (\(l, x, l') -> (reverse l, x, l')) $ splits [] expr

    -- Split list on operands.
    -- First argument and first element in tuple are reversed.
    splits _ []                     = []
    splits xs (y@(Operand _) : ys') = (xs, y, ys') : splits (y : xs) ys'
    splits xs (y : ys)              = splits (y : xs) ys

-- | Reconstruct the calculation history from the calculation tree.
history :: Calculation -> [([Integer], Maybe Operator)]
history calc = reverse $ reconstruct calc
  where
    reconstruct (Calculation { element = Operand x }) = [([x], Nothing)]

    reconstruct (Calculation { element = Operator Negate
                             , result = x
                             , inputs = [c]
                             })
      | h@((_ : xs, _) : _) <- reconstruct c = (x : xs, Just Negate) : h
      | otherwise = error "no input for Negate"

    reconstruct (Calculation { element = Operator op
                             , result = x
                             , inputs = [c, c']
                             })

      | h@((s, _) : _) <- reconstruct c
      , h'@((_ : xs', _) : _) <- reconstruct c' =

          -- Most recent stack is the result of the operator.
          [ (x : xs', Just op) ] ++

          -- The second argument to the binary operator is evaluated
          -- while the stack from the first argument is at the bottom.
          map (\(s', op') -> (s' ++ s, op')) h' ++

          -- The history for the first argument is included as is.
          h

      | otherwise = error "no inputs for binary operator"

    reconstruct c = error $ "malformed calculation " ++ show c
