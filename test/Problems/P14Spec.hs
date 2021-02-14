module Problems.P14Spec (spec) where

import           Problems.P14
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "dupli" $ do
    prop "duplicates elements of list" $
      let doubled [] []            = True
          doubled (x:xs) (y:y':ys) = x == y && y == y' && doubled xs ys
          doubled _ _              = False
      in \l -> dupli l `shouldSatisfy` doubled (l :: [Int])

  describe "Examples" $ do
    it "dupli [1, 2, 3]" $ do
      dupli [1, 2, 3] `shouldBe` [1,1,2,2,3,3 :: Int]
