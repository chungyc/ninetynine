module Problems.P12Spec (spec) where

import           Problems.P11
import           Problems.P11.Definitions
import           Problems.P12
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "decodeModified" $ do
    prop "restores original list from its encoding" $
      \l -> decodeModified (encodeModified l) `shouldBe` (l :: [Int])

  describe "Examples" $ do
    it "decodeModified [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']" $ do
      decodeModified [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']
        `shouldBe` "aaaabccaadeeee"
