module Problems.P05Spec (spec) where

import           Problems.P05
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "myReverse" $ do
    prop "reverses a list" $
      \l -> let naiveReverse []     = []
                naiveReverse (x:xs) = (naiveReverse xs) ++ [x]
            in myReverse l `shouldBe` naiveReverse (l :: [Int])

    prop "returns original from reversed list" $
      \l -> myReverse (myReverse l) `shouldBe` (l :: [Int])

  describe "Examples" $ do
    it "myReverse \"A man, a plan, a canal, panama!\"" $ do
      myReverse "A man, a plan, a canal, panama!" `shouldBe` "!amanap ,lanac a ,nalp a ,nam A"

    it "myReverse [1,2,3,4]" $ do
      myReverse [1,2,3,4] `shouldBe` ([4,3,2,1] :: [Int])
