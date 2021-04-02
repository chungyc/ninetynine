{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P49Spec (spec) where

import qualified Problems.P49  as Problem
import qualified Solutions.P49 as Solution
import           Test.Hspec

properties :: (Int -> [String]) -> String -> Spec
properties gray name = do
  describe name $ do
    it "constructs 1-bit Gray code" $ do
      gray 1 `shouldBe` ["0", "1"]

    it "constructs 2-bit Gray code" $ do
      gray 2 `shouldBe` ["00", "01", "11", "10"]

    it "constructs 3-bit Gray code" $ do
      gray 3  `shouldBe` ["000", "001", "011", "010", "110", "111", "101", "100"]

    -- There are general properties that hold for Gray codes,
    -- but this problem only demands that the function reproduces the examples.

spec :: Spec
spec = parallel $ do
  properties Problem.gray "gray"
  describe "From solutions" $ do
    properties Solution.gray "gray"
