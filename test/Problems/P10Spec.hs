module Problems.P10Spec (spec) where

import           Problems.P10
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "encode" $ do
    prop "decodes into original list" $
      \l -> (concat $ map (uncurry replicate) $ encode l) `shouldBe` (l :: [Int])

    prop "keeps consecutive encoded elements distinct" $
      \l -> let distinct (x : ys@(y : _))
                  | x == y    = False
                  | otherwise = distinct ys
                distinct _ = True
            in encode (l :: [Int]) `shouldSatisfy` distinct . (map snd)

  describe "Examples" $ do
    it "encode \"aaaabccaadeeee\"" $ do
      encode "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
