{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P10Spec (spec) where

import qualified Problems.P10          as Problem
import qualified Solutions.P10         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Int] -> [(Int, Int)]) -> String -> Spec
properties encode name = describe name $ do
  prop "decodes into original list" $ \l ->
    encode l `shouldSatisfy` (==) l . concatMap (uncurry replicate)

  prop "encodes consecutive duplicates to single encoding" $ \xs x z y ys (Positive k) ->
    x /= z && z /= y ==>
    let xs' = xs ++ [x]
        ys' = y : ys
        vs = xs' ++ replicate k z ++ ys'
    in counterexample (show vs) $
       encode vs `shouldBe` encode xs' ++ [(k,z)] ++ encode ys'

examples :: Spec
examples = describe "Examples" $ do
  it "encode \"aaaabccaadeeee\"" $ do
    encode "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

  where encode = Problem.encode

spec :: Spec
spec = parallel $ do
  properties Problem.encode "encode"
  examples
  describe "From solutions" $ do
    properties Solution.encode "encode"
