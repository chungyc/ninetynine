module Solutions.ArithmeticSpec (spec) where

import           Solutions.Arithmetic
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "dividesBy" $ do
    prop "if and only if first argument divides by second argument" $
      \(Positive n) -> \(Positive m) ->
        classify (n `dividesBy` m) "divisor" $
        dividesBy n m `shouldBe` (n `div` m :: Integer) * m == n

  describe "primes" $ do
    prop "has elements which are prime numbers" $
      \(NonNegative n) ->
        let p = head (drop n primes) :: Integer
        in p `shouldSatisfy` isPrime

    prop "has no numbers which are prime between consecutive elements" $
      \(NonNegative n) ->
        let (p:q:_) = drop n primes :: [Integer]
        in conjoin (map (flip shouldNotSatisfy isPrime) [(p+1)..(q-1)])

  where isPrime p = not $ any (dividesBy p) [2..(p-1)]
