{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P05Spec (spec) where

import qualified Problems.P05          as Problem
import qualified Solutions.P05         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: ([Int] -> [Int]) -> String -> Spec
properties myReverse name = describe name $ do
  prop "front becomes back" $ \l -> \x ->
    myReverse (x:l) `shouldBe` myReverse l ++ [x]

  prop "back becomes front" $ \l -> \x ->
    myReverse (l ++ [x]) `shouldBe` x : myReverse l

  prop "is inverse of itself" $ \l ->
    (myReverse . myReverse) l `shouldBe` l

examples :: Spec
examples = describe "Examples" $ do
  it "myReverse \"A man, a plan, a canal, panama!\"" $ do
    myReverse "A man, a plan, a canal, panama!" `shouldBe` "!amanap ,lanac a ,nalp a ,nam A"

  it "myReverse [1,2,3,4]" $ do
    myReverse [1,2,3,4] `shouldBe` [4,3,2,1 :: Int]

  where myReverse = Problem.myReverse

spec :: Spec
spec = parallel $ do
  properties Problem.myReverse "myReverse"
  examples
  describe "From solutions" $ do
    properties Solution.myReverse "myReverse"
