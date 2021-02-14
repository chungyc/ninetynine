module Problems.P15Spec (spec) where

import           Problems.P15
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "repli" $ do
    prop "repeats elements given number of times" $
      let naiveRepli l n = concat $ map (replicate n) l
      in \l -> \(NonNegative n) -> repli l n `shouldBe` naiveRepli (l :: [Int]) n

  describe "Examples" $ do
    it "repli \"abc\" 3" $ do
      repli "abc" 3 `shouldBe` "aaabbbccc"
