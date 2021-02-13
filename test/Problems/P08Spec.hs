module Problems.P08Spec (spec) where

import           Problems.P08
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "compress" $ do
    prop "leaves no consecutive duplicates" $
      \l -> let consecutive []           = False
                consecutive [_]          = False
                consecutive (x:xs@(y:_)) = x == y || consecutive xs
            in classify (not . consecutive $ l) "trivial" $
               compress (l :: [Int]) `shouldSatisfy` not . consecutive

    prop "leaves elements in same order" $
      \l -> let consume _ [] = []
                consume x (y:ys)
                  | x == y    = consume x ys
                  | otherwise = (y:ys)
                sameOrder ([], []) = True
                sameOrder ([], _)  = False
                sameOrder (_, [])  = False
                sameOrder (x:xs, y:ys)
                  | x == y    = sameOrder (consume x xs, consume y ys)
                  | otherwise = False
            in (l :: [Int], compress l) `shouldSatisfy` sameOrder

  describe "Examples" $ do
    it "compress \"aaaabccaadeeee\"" $ do
      compress "aaaabccaadeeee" `shouldBe` "abcade"
