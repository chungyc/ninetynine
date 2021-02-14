module Problems.P13Spec (spec) where

import           Problems.P11
import           Problems.P11.Definitions
import           Problems.P13
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "encodeDirect" $ do
    prop "is same as encodeModified" $
      \l -> encodeDirect l `shouldBe` encodeModified (l :: [Int])

  describe "Examples" $ do
    it "encodeDirect \"aaaabccaadeeee\"" $ do
      encodeDirect "aaaabccaadeeee" `shouldBe`
        [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']
