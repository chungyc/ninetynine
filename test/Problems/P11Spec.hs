module Problems.P11Spec (spec) where

import           Problems.P10
import           Problems.P11
import           Problems.P11.Definitions
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "encodeModified" $ do
    prop "is same as encode, but with single elements not in sublist" $
      \l -> let similar ([], []) = True
                similar ((n, x) : xs, Single y : ys)
                  | n == 1 && x == y = similar (xs, ys)
                  | otherwise = False
                similar ((n, x) : xs, Multiple m y : ys)
                  | n == m && x == y = similar (xs, ys)
                  | otherwise = False
                similar _ = False
            in (encode (l :: [Int]), encodeModified l) `shouldSatisfy` similar

    prop "does not confuse single element as multiple elements" $
      \l -> let multipleone (Multiple 1 _) = True
                multipleone _              = False
            in encodeModified (l :: [Int]) `shouldNotSatisfy` any multipleone

  describe "Examples" $ do
    it "encodeModified \"aaaabccaadeeee\"" $ do
      encodeModified "aaaabccaadeeee" `shouldBe`
        [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
