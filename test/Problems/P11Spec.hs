{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P11Spec (spec) where

import           Problems.Lists
import qualified Problems.P11          as Problem
import qualified Solutions.P11         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Int] -> [Encoding Int]) -> String -> Spec
properties encodeModified name = describe name $ do
  prop "encodes nothing" $ do
    encodeModified [] `shouldBe` []

  prop "multiple element is multiple" $ \l ->
    encodeModified l
    `shouldSatisfy` all (\x -> case x of Multiple n _ -> n > 1; _ -> True)

  prop "encodes single element" $ \xs x z y ys ->
    x /= z && z /= y ==>
    let xs' = xs ++ [x]
        ys' = y : ys
        vs = xs' ++ [z] ++ ys'
    in counterexample (show vs) $
       encodeModified vs `shouldBe` encodeModified xs' ++ [Single z] ++ encodeModified ys'

  prop "encode consecutive duplicates" $ \xs x z y ys (Positive k) ->
    k > 1 ==>
    x /= z && z /= y ==>
    let xs' = xs ++ [x]
        ys' = y : ys
        vs = xs' ++ replicate k z ++ ys'
    in counterexample (show vs) $
       encodeModified vs `shouldBe` encodeModified xs' ++ [Multiple k z] ++ encodeModified ys'

examples :: Spec
examples = describe "Examples" $ do
  it "encodeModified \"aaaabccaadeeee\"" $ do
    encodeModified "aaaabccaadeeee" `shouldBe`
      [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

  where encodeModified = Problem.encodeModified

spec :: Spec
spec = parallel $ do
  properties Problem.encodeModified "encodeModified"
  examples
  describe "From solutions" $ do
    properties Solution.encodeModified "encodeModified"
