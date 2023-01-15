{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P51Spec (spec) where

import           Data.Maybe            (fromJust)
import qualified Problems.P51          as Problem
import qualified Solutions.P51         as Solution
import           System.Random         (StdGen, mkStdGen)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (StdGen -> Int -> [Bool] -> [Bool], [Bool] -> [Bool], [Bool] -> [Bool])
           -> (String, String, String)
           -> Spec
properties
  (corrupt, errorCorrectingEncode, errorCorrectingDecode)
  (nameCorrupt, nameEncode, nameDecode) = do
  describe nameCorrupt $ do
    prop "has same length" $
      \(RNG gen) -> \l -> forAll (chooseInt (0, length l)) $ \n ->
        corrupt gen n l `shouldSatisfy` (==) (length l) . length

    prop "has expected number of errors" $
      \(RNG gen) -> \l -> forAll (chooseInt (0, length l)) $ \n ->
        corrupt gen n l `shouldSatisfy` (==) n . fromJust . countErrors l

  describe (nameEncode ++ " and " ++ nameDecode) $ do
    prop "encodes and decodes back to original input" $
      \l -> (errorCorrectingDecode . errorCorrectingEncode) l `shouldBe` l

    prop "encodes and decodes back to original input with one error" $
      \(RNG gen) -> \l ->
        (errorCorrectingDecode . corrupt gen 1 . errorCorrectingEncode) l `shouldBe` l

examples :: Spec
examples = describe "Examples" $ do
  it "corrupt (mkStdGen 111) 2 [False, True, True, False, True]" $ do
    corrupt (mkStdGen 111) 2 [False, True, True, False, True]
      `shouldSatisfy`  (==) 2 . fromJust . countErrors [False, True, True, False, True]

  it "errorCorrectingDecode . errorCorrectingEncode $ [False, False, True, False]" $ do
    (errorCorrectingDecode . errorCorrectingEncode) [False, False, True, False]
      `shouldBe` [False, False, True, False]

  it "errorCorrectingDecode $ corrupt (mkStdGen 111) 1 $ errorCorrectingEncode [True, False, False, True, False]" $ do
    (errorCorrectingDecode $ corrupt (mkStdGen 111) 1 $ errorCorrectingEncode [True, False, False, True, False])
      `shouldBe` [True, False, False, True, False]

  where corrupt = Problem.corrupt
        errorCorrectingEncode = Problem.errorCorrectingEncode
        errorCorrectingDecode = Problem.errorCorrectingDecode

spec :: Spec
spec = parallel $ do
  properties
    (Problem.corrupt, Problem.errorCorrectingEncode, Problem.errorCorrectingDecode)
    ("corrupt", "errorCorrectingEncode", "errorCorrectingDecode")
  examples
  describe "From solutions" $ do
    properties
      (Solution.corrupt, Solution.errorCorrectingEncode, Solution.errorCorrectingDecode)
      ("corrupt", "encode", "decode")

-- | Count differences from two lists of the same length.
--
-- If the lists are not the same length, returns Nothing.
countErrors :: [Bool] -> [Bool] -> Maybe Int
countErrors [] [] = Just 0
countErrors (x:xs) (x':xs')
  | x == x' = countErrors xs xs'
  | otherwise = do
      c <- countErrors xs xs'
      return $ c+1
countErrors _ _ = Nothing

newtype RNG = RNG StdGen deriving Show

instance Arbitrary RNG where
  arbitrary = RNG . mkStdGen <$> arbitrary
