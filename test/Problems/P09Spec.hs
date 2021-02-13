module Problems.P09Spec (spec) where

import           Problems.P08
import           Problems.P09
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "pack" $ do
    prop "places identical elements in a sublist" $
      \l -> let repeated []           = True
                repeated [_]          = True
                repeated (x:ys@(y:_)) = x == y && repeated ys
            in classify (l == compress l) "trivial" $
               pack (l :: [Int]) `shouldSatisfy` all repeated

    prop "has expected sublist for each consecutive segment" $
      \l -> classify (l == compress l) "trivial" $
            map head (pack l) `shouldBe` compress (l :: [Int])

    prop "has the same elements but in sublists" $
      \l -> classify (l == compress l) "trivial" $
            (concat .  pack) l `shouldBe` (l :: [Int])

  describe "Examples" $ do
    it "pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']" $ do
      pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
        `shouldBe` ["aaaa","b","cc","aa","d","eeee"]
