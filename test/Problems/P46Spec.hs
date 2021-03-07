module Problems.P46Spec (spec) where

import           Data.List             (sort)
import           Problems.Logic
import qualified Problems.P46          as Problem
import qualified Solutions.P46         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: Functions -> String -> Spec
properties fs name = do
  describe name $ do
    prop "and' iff both true" $
      \a -> \b -> and' a b `shouldBe` toInt a + toInt b == 2

    prop "or' iff either true" $
      \a -> \b -> or' a b `shouldBe` toInt a + toInt b > 0

    prop "nand' is not and'" $
      \a -> \b -> nand' a b `shouldBe` not (and' a b)

    prop "nor' is not or'" $
      \a -> \b -> nor' a b `shouldBe` not (or' a b)

    prop "xor' iff only one true" $
      \a -> \b -> xor' a b `shouldBe` toInt a + toInt b == 1

    prop "impl' implies consequent is true if antecedent is true" $
      \a -> impl' True a `shouldBe` a == True

    prop "impl' does not care if antecedent is false" $
      \a -> impl' False a `shouldBe` True

    prop "equ' iff the same" $
      \a -> \b -> equ' a b `shouldBe` a == b

    prop "table" $
      \(Fn2 f) -> \a -> \b -> table f `shouldSatisfy` elem (a, b, f a b)

    where Functions
            { getTable = table
            , getAnd = and'
            , getOr = or'
            , getNand = nand'
            , getNor = nor'
            , getXor = xor'
            , getImpl = impl'
            , getEqu = equ' } = fs
          toInt False = 0 :: Int
          toInt True  = 1 :: Int

examples :: Spec
examples = do
  describe "Examples" $ do
    it "table (\a b -> (and' a (or' a b)))" $ do
      sort (table $ \a b -> (and' a $ or' a b))
        `shouldBe` [(False,False,False),(False,True,False),(True,False,True),(True,True,True)]

  where table = Problem.table
        and'  = Problem.and'
        or'   = Problem.or'

spec :: Spec
spec = parallel $ do
  properties Problem.functions "boolean functions"
  examples
  describe "From solutions" $ do
    properties Solution.functions "boolean functions"
