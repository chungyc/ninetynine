{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P08Spec (spec) where

import qualified Problems.P08          as Problem
import qualified Solutions.P08         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Int] -> [Int]) -> String -> Spec
properties compress name = describe name $ do
  prop "compresses nothing" $
    compress [] `shouldBe` []

  prop "compresses singleton" $ \x ->
    compress [x] `shouldBe` [x]

  prop "is idempotent" $ \xs ->
    compress (compress xs) `shouldBe` compress xs

  prop "does not discard element" $ \xs x xs' ->
    compress (xs ++ [x] ++ xs') `shouldSatisfy` elem x

  prop "removes consecutive duplicates" $
    \xs x z y ys (Positive k) ->
      x /= z && z /= y ==>
      let xs' = xs ++ [x]
          ys' = y : ys
          vs = xs' ++ replicate k z ++ ys'
      in counterexample (show vs) $
         compress vs `shouldBe` compress xs' ++ [z] ++ compress ys'

  prop "maintains order" $ \xs x y ys ->
    let xs' = xs ++ [x]
        ys' = y : ys
        vs = xs' ++ ys'
    in counterexample (show vs) $
       case x == y of
         True -> compress vs `shouldBe` compress xs' ++ drop 1 (compress ys')
         False -> compress vs `shouldBe` compress xs' ++ compress ys'

examples :: Spec
examples = describe "Examples" $ do
  it "compress \"aaaabccaadeeee\"" $ do
    compress "aaaabccaadeeee" `shouldBe` "abcade"

  where compress = Problem.compress

spec :: Spec
spec = parallel $ do
  properties Problem.compress "compress"
  examples
  describe "From solutions" $ do
    properties Solution.compress "compress"
