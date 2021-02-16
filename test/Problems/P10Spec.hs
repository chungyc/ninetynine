module Problems.P10Spec (spec) where

import qualified Problems.P10          as Problem
import qualified Solutions.P10         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: ([Int] -> [(Int, Int)]) -> String -> Spec
properties encode name = do
  describe name $ do
    prop "decodes into original list" $
      \l -> (concat $ map (uncurry replicate) $ encode l) `shouldBe` l

    prop "keeps consecutive encoded elements distinct" $
      \l -> let distinct (x : ys@(y : _))
                  | x == y    = False
                  | otherwise = distinct ys
                distinct _ = True
            in encode l `shouldSatisfy` distinct . (map snd)

examples :: Spec
examples = do
  describe "Examples" $ do
    it "encode \"aaaabccaadeeee\"" $ do
      encode "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

  where encode l = Problem.encode l

spec :: Spec
spec = do
  properties Problem.encode "encode"
  examples
  describe "From solutions" $ do
    properties Solution.encode "encode"
