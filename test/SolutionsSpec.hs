{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Description : Ninety-Nine Haskell Solutions
Maintainer  : dev@chungyc.org

The [99 Haskell Problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems) expressed in testable form.
-}
module SolutionsSpec (spec) where

import           Generic.Random
import           Solutions
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

instance Arbitrary a => Arbitrary (NestedList a) where
  arbitrary = genericArbitraryRec (9 % 8 % ()) `withBaseCase` (Elem <$> arbitrary)

spec :: Spec
spec = do
  describe "Problem 10" $ do
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
