{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P19Spec (spec) where

import qualified Problems.P19          as Problem
import qualified Solutions.P19         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Int] -> Int -> [Int]) -> String -> Spec
properties rotate name = describe name $ do
  prop "rotation by zero is identity" $ \(NonEmpty xs) ->
    rotate xs 0 `shouldBe` xs

  prop "rotates by one" $ \(NonEmpty xs) ->
    rotate xs 1 `shouldBe` tail xs ++ [head xs]

  prop "rotates by negative one" $ \(NonEmpty xs) ->
    rotate xs (-1) `shouldBe` last xs : init xs

  prop "rotates by n equivalent to (rotate 1) n times" $ \(NonEmpty xs) (Positive n) ->
    rotate xs n `shouldBe` (iterate (`rotate` 1) xs !! n)

  prop "rotates by -n equivalent to (rotate -1) n times" $ \(NonEmpty xs) (Positive n) ->
    rotate xs (-n) `shouldBe` (iterate (`rotate` (-1)) xs !! n)

  prop "rotate _ (-n) is inverse of rotate _ n" $ \xs n ->
    rotate (rotate xs n) (-n) `shouldBe` xs

examples :: Spec
examples = describe "Examples" $ do
  it "rotate \"abcdefgh\" 3" $ do
    rotate "abcdefgh" 3 `shouldBe` "defghabc"

  it "rotate \"abcdefgh\" (-2)" $ do
    rotate "abcdefgh" (-2) `shouldBe` "ghabcdef"

  where rotate = Problem.rotate

spec :: Spec
spec = parallel $ do
  properties Problem.rotate "rotate"
  examples
  describe "From solutions" $ do
    properties Solution.rotate "rotate"
