{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.TutorialSpec (spec) where

import qualified Problems.Tutorial     as Problem
import qualified Solutions.Tutorial    as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Integer -> Integer) -> String -> Spec
properties sumNumbers name = describe name $ do
  prop "is one when adding just one" $
    sumNumbers 1 `shouldBe` 1

  prop "adds a number to the sum" $ \(Positive n) ->
    sumNumbers (n+1) `shouldBe` sumNumbers n + (n+1)

examples :: Spec
examples = describe "Examples" $ do
  it "sumNumbers 5 == 1 + 2 + 3 + 4 + 5" $ do
    sumNumbers 5 == 1 + 2 + 3 + 4 + 5 `shouldBe` True

  it "sumNumbers 100" $ do
    sumNumbers 100 `shouldBe` 5050

  where sumNumbers = Problem.sumNumbers

spec :: Spec
spec = parallel $ do
  properties Problem.sumNumbers "sumNumbers"
  examples
  describe "From solutions" $ do
    properties Solution.sumNumbers "sumNumbers"
    properties Solution.sumNumbers' "sumNumbers'"
    properties Solution.sumNumbers'' "sumNumbers''"
