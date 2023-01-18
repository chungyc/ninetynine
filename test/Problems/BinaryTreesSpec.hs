{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.BinaryTreesSpec (spec) where

import           Problems.BinaryTrees
import           Problems.BinaryTrees.Arbitrary ()
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

spec :: Spec
spec = describe "compare" $ do
  prop "is reflexive" $ \t ->
    leq t t `shouldBe` True

  prop "is antisymmetric" $ \t v ->
    classify (t == v) "equal" $
    eq t v `shouldBe` leq t v && leq v t

  prop "is transitive" $ \t v u ->
    leq t v && leq v u ==> leq t u `shouldBe` True

  prop "is total" $ \t v ->
    leq t v || leq v t `shouldBe` True

  where eq t v = t == (v :: Tree Int)
        leq t v = t < (v :: Tree Int) || t == v
