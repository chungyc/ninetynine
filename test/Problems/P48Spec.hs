{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P48Spec (spec) where

import           Data.List             (nub, sort)
import           Problems.P46
import qualified Problems.P48          as Problem
import qualified Solutions.P48         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Int -> ([Bool] -> Bool) -> [[Bool]]) -> String -> Spec
properties tablen name = describe name $ do
  prop "satisfies logical expression" $
    \f -> \(Tiny n) ->
      tablen n (applyFun f)
      `shouldSatisfy` all (\xs -> applyFun f (init xs) == last xs)

  prop "assigns expected number of variables" $
    \f -> \(Tiny n) ->
      tablen n (applyFun f)
      `shouldSatisfy` all (\x -> length x == n + 1)

  prop "assigns all values to variables" $
    \f -> \(Tiny n) ->
      tablen n (applyFun f)
      `shouldSatisfy` (==) (2^n) . length . nub . map init
      -- There are 2^n combinations of n booleans,
      -- so there should be 2^n distinct variable assignments.

examples :: Spec
examples = describe "Examples" $ do
  it "tablen 3 (\\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)" $ do
    sort (tablen 3 $
           \x -> case x of
             [a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c
             _       -> undefined)
      `shouldMatchList` [ [False, False, False, False]
                        , [False, False, True,  False]
                        , [False, True,  False, False]
                        , [False, True,  True,  True]
                        , [True,  False, False, False]
                        , [True,  False, True,  True]
                        , [True,  True,  False, False]
                        , [True,  True,  True,  True]
                        ]

  where tablen = Problem.tablen

spec :: Spec
spec = parallel $ do
  properties Problem.tablen "tablen"
  examples
  describe "From solutions" $ do
    properties Solution.tablen "tablen"

newtype Tiny = Tiny Int deriving (Show)

instance Arbitrary Tiny where
  arbitrary = Tiny <$> resize 10 arbitrarySizedNatural
  shrink (Tiny n) = (map Tiny) $ shrink n
