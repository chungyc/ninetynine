{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P70Spec (spec) where

import           Problems.MultiwayTrees
import           Problems.MultiwayTrees.Arbitrary ()
import qualified Problems.P70                     as Problem
import qualified Solutions.P70                    as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties
  :: (String -> MultiwayTree Char, MultiwayTree Char -> String)
  -> (String, String)
  -> Spec
properties
  (stringToMultitree, multitreeToString)
  (nameStringToMultitree, nameMultitreeToString) = do
  describe nameStringToMultitree $ do
    prop "is inverse of multitreeToString" $
      \(CharTree t) -> (stringToMultitree . multitreeToString) t `shouldBe` t

  describe nameMultitreeToString $ do
    prop "maps to string from singleton tree" $ forAll letters $ \c ->
      multitreeToString (MultiwayTree c []) `shouldBe` [c] ++ "^"

    prop "maps to string in depth-first order" $
      \(CharTree t@(MultiwayTree c ts)) ->
        multitreeToString t `shouldBe` [c] ++ concatMap multitreeToString ts ++ "^"

examples :: Spec
examples = do
  describe "Examples" $ do
    it "stringToMultitree \"afg^^c^bd^e^^^\" == multitree5" $ do
      stringToMultitree "afg^^c^bd^e^^^" `shouldBe` multitree5

    it "multitreeToString multitree5" $ do
      multitreeToString multitree5 `shouldBe` "afg^^c^bd^e^^^"

  where stringToMultitree = Problem.stringToMultitree
        multitreeToString = Problem.multitreeToString

spec :: Spec
spec = parallel $ do
  properties
    (Problem.stringToMultitree, Problem.multitreeToString)
    ("stringToMultitree", "multitreeToString")
  examples
  describe "From solutions" $ do
    properties
      (Solution.stringToMultitree, Solution.multitreeToString)
      ("stringToMultitree", "multitreeToString")

-- | Generates a letter.
-- It will not generate the special character '^'.
letters :: Gen Char
letters = choose ('a', 'z')

-- | Multiway tree with character values.
-- No characters should be the special character '^'.
newtype CharTree = CharTree (MultiwayTree Char) deriving (Show)

instance Arbitrary CharTree where
  arbitrary = CharTree <$> sized gen
    where gen 0 = frequency
                  [ (10, MultiwayTree <$> letters <*> return [])
                  , (1, MultiwayTree <$> letters <*> subtreeList 0)
                  ]
          gen n = frequency
                  [ (1, MultiwayTree <$> letters <*> return [])
                  , (10, MultiwayTree <$> letters <*> subtreeList n)
                  ]
          subtreeList n = do
            k <- chooseInt (0, n)
            let s = case k of 0 -> 0; _ -> n `div` k
            vectorOf k $ gen s

  shrink (CharTree t) = map CharTree $ shrink' t
    where shrink' (MultiwayTree _ []) = []
          shrink' (MultiwayTree x xs) =
            [ MultiwayTree x [] ] ++
            xs ++
            map (MultiwayTree x) (shrinkList shrink' xs)
