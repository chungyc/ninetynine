{-|
Copyright: Copyright (C) 2021 Yoo Chung
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
spec = do
  describe "compare" $ do
    prop "is reflexive" $
      \t -> leq t t `shouldBe` True

    prop "is antisymmetric" $
      \t -> \v -> leq t v && leq v t ==> eq t v `shouldBe` True

    prop "is transitive" $
      \t -> \v -> \u -> leq t v && leq v u ==> leq t u `shouldBe` True

    prop "is total" $
      \t -> \v -> classify (eq t v) "equal" $
                  leq t v || leq v t `shouldBe` True

  where eq t v = compare t (v :: Tree Int) == EQ
        leq t v = compare t (v :: Tree Int) == LT || compare t v == EQ
