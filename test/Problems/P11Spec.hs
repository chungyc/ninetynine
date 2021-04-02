{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P11Spec (spec) where

import           Problems.Lists
import           Problems.P10
import qualified Problems.P11          as Problem
import qualified Solutions.P11         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: ([Int] -> [Encoding Int]) -> String -> Spec
properties encodeModified name = do
  describe name $ do
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

examples :: Spec
examples = do
  describe "Examples" $ do
    it "encodeModified \"aaaabccaadeeee\"" $ do
      encodeModified "aaaabccaadeeee" `shouldBe`
        [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

  where encodeModified l = Problem.encodeModified l

spec :: Spec
spec = parallel $ do
  properties Problem.encodeModified "encodeModified"
  examples
  describe "From solutions" $ do
    properties Solution.encodeModified "encodeModified"
