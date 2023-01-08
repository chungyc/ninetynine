{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P34Spec (spec) where

import           Data.List             (genericLength)
import           Problems.P33
import qualified Problems.P34          as Problem
import qualified Solutions.P34         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Integer -> Integer) -> String -> Spec
properties totient name = describe name $ do
  prop "is count of lesser than or equal numbers that are coprime" $
    \(Positive n) -> totient n `shouldBe` genericLength (filter (coprime n) [1..n])

examples :: Spec
examples = describe "Examples" $ do
  it "totient 10" $ do
    totient 10 `shouldBe` 4

  where totient n = Problem.totient n:: Int

spec :: Spec
spec = parallel $ do
  properties Problem.totient "totient"
  examples
  describe "From solution" $ do
    properties Solution.totient         "totient"
    properties Solution.totientFiltered "totientFiltered"
