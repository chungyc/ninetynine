{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P98Spec (spec) where

import           Control.Monad
import           Data.List             (group, transpose)
import qualified Problems.P98          as Problem
import qualified Solutions.P98         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([[Int]] -> [[Int]] -> Maybe [[Bool]]) -> String -> Spec
properties nonogram name = describe name $ do
  modifyMaxSize (const 15) $ do
    prop "is consistent with puzzle" $
      \(Bitmap b) -> counterexample ("rows=" ++ show (getRows b)) $
                     counterexample ("columns=" ++ show (getColumns b)) $
                     nonogram (getRows b) (getColumns b) `shouldSatisfy`
                     \(Just b') -> getRows b == getRows b' && getColumns b == getColumns b'

examples :: Spec
examples = do
  describe "Examples" $ do
    it "nonogram rows columns" $
      let rows = [[3],[2,1],[3,2],[2,2],[6],[1,5],[6],[1],[2]]
          columns = [[1,2],[3,1],[1,5],[7,1],[5],[3],[4],[3]]
      in nonogram rows columns `shouldBe`
         Just [ [False,True, True, True, False,False,False,False]
              , [True, True, False,True, False,False,False,False]
              , [False,True, True, True, False,False,True, True ]
              , [False,False,True, True, False,False,True, True ]
              , [False,False,True, True, True, True, True, True ]
              , [True, False,True, True, True, True, True, False]
              , [True, True, True, True, True, True, False,False]
              , [False,False,False,False,True, False,False,False]
              , [False,False,False,True, True, False,False,False]
              ]

  where nonogram = Problem.nonogram

spec :: Spec
spec = parallel $ do
  properties Problem.nonogram "nonogram"
  examples
  describe "From solutions" $ do
    properties Solution.nonogram "nonogram"

getRows :: [[Bool]] -> [[Int]]
getRows rs = map lengths rs
  where lengths bs = map length $ filter head $ group bs

getColumns :: [[Bool]] -> [[Int]]
getColumns cs = getRows $ transpose cs

newtype Bitmap = Bitmap [[Bool]] deriving (Eq, Show)

instance Arbitrary Bitmap where
  arbitrary = do
    n <- liftM (2+) $ getSize
    m <- chooseInt (2,n)
    liftM Bitmap $ vectorOf m $ vectorOf n arbitrary
