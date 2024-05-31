{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P38Spec (spec) where

import           Data.List             (genericLength)
import           Problems.P35
import           Problems.P37
import qualified Problems.P38          as Problem
import qualified Solutions.P38         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: [Integer] -> String -> Spec
properties highlyTotientNumbers name = describe name $ do
  prop "includes 1" $ do
    head highlyTotientNumbers `shouldBe` 1

  modifyMaxSize (const 14) $ do
    prop "members have more solutions than lesser totient numbers" $ \(Positive k) ->
      let n = highlyTotientNumbers !! k
      in forAll (chooseInteger (1,n-1)) $ \m ->
         counterexample ("n=" ++ show n) $
         counterexample ("m=" ++ show m) $
         counterexample ("solutions for n=" ++ show (countSolutions n)) $
         counterexample ("solutions for m=" ++ show (countSolutions m)) $
         countSolutions n `shouldSatisfy` (<) (countSolutions m)

    prop "does not skip highly totient number" $ \(Positive k) ->
      let (n,n') = zip highlyTotientNumbers (tail highlyTotientNumbers) !! k
      in forAll (chooseInteger (n+1,n'-1)) $ \m ->
         counterexample ("n=" ++ show n) $
         counterexample ("m=" ++ show m) $
         counterexample ("solutions for n=" ++ show (countSolutions n)) $
         counterexample ("solutions for m=" ++ show (countSolutions m)) $
         countSolutions m `shouldSatisfy` (>=) (countSolutions n)

examples :: Spec
examples = describe "Examples" $ do
  it "take 10 highlyTotientNumbers" $ do
    take 10 highlyTotientNumbers `shouldBe` [1,2,4,8,12,24,48,72,144,240]

  where highlyTotientNumbers = Problem.highlyTotientNumbers :: [Int]

spec :: Spec
spec = parallel $ do
  properties Problem.highlyTotientNumbers "highlyTotientNumbers"
  examples
  describe "From solution" $ do
    properties Solution.highlyTotientNumbers "highlyTotientNumbers"

countSolutions :: Integer -> Integer
countSolutions n = genericLength $ filter ((==) n . totient') [1..bound]
  where bound = product $ map (1+) $ 1 : primeFactors n
  -- All solutions for \(\phi(x)=n\) must be in @[1..bound]@.
  -- The proof is in the comments for 'Solutions.P38.tally'.
