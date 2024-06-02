{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P09Spec (spec) where

import qualified Problems.P09          as Problem
import qualified Solutions.P09         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Int] -> [[Int]]) -> String -> Spec
properties pack name = describe name $ do
  prop "sublists are packed from list" $ \xs ->
    pack xs `shouldSatisfy` (==) xs . concat

  prop "sublists contain identical elements" $ \xs ->
    pack xs `shouldSatisfy` all (\l -> and [ x == y | x <- l, y <- l])

  prop "identical elements do not span consecutive sublists" $
    \xs x z y ys (Positive k) ->
      x /= z && z /= y ==>
      let xs' = xs ++ [x]
          ys' = y : ys
          vs = xs' ++ replicate k z ++ ys'
      in counterexample (show vs) $
         pack vs `shouldBe` pack xs' ++ [replicate k z] ++ pack ys'

examples :: Spec
examples = describe "Examples" $ do
  it "pack \"aaaabccaadeeee\"" $ do
    pack "aaaabccaadeeee" `shouldBe` ["aaaa","b","cc","aa","d","eeee"]

  where pack = Problem.pack

spec :: Spec
spec = parallel $ do
  properties Problem.pack "pack"
  examples
  describe "From solutions" $ do
    properties Solution.pack  "pack"
    properties Solution.pack' "pack'"
