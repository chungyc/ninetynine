{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P49Spec (spec) where

import qualified Problems.P49          as Problem
import qualified Solutions.P49         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Int -> [String]) -> String -> Spec
properties gray name = describe name $ do
  modifyMaxSize (const 15) $ do
    prop "constructs 0-bit gray code" $ do
      gray 0 `shouldBe` [""]

    prop "constructs n-bit gray code" $ \(Positive n) ->
      gray n `shouldBe`
      map ('0':) (gray $ n-1) ++ map ('1':) (reverse $ gray $ n-1)

examples :: Spec
examples = describe "Examples" $ do
  it "gray 1" $ do
    gray 1 `shouldBe` ["0", "1"]

  it "gray 2" $ do
    gray 2 `shouldBe` ["00", "01", "11", "10"]

  it "gray 3" $ do
    gray 3  `shouldBe` ["000", "001", "011", "010", "110", "111", "101", "100"]

  where gray = Problem.gray

spec :: Spec
spec = parallel $ do
  properties Problem.gray "gray"
  examples
  describe "From solutions" $ do
    properties Solution.gray "gray"
