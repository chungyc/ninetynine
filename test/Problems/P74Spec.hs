{-|
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P74Spec (spec) where

import           Problems.P74          (askGoldbach', withFixedInput)
import qualified Problems.P74          as Problem
import           Solutions.P40         (goldbach)
import qualified Solutions.P74         as Solution
import           System.IO
import           System.Process        (createPipe)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Handle -> Handle -> IO ()) -> String -> Spec
properties askGoldbach name = describe name $ do
  prop "prints Goldbach decomposition" $
    \(Positive n') -> n' > 1 ==>
                      let n = 2 * n' :: Int
                          (a,b) = goldbach n
                      in getOutput (\hOut -> withFixedInput (show n) hOut askGoldbach)
                         `shouldReturn` (show n) ++ "=" ++ (show a) ++ "+" ++ (show b)

examples :: Spec
examples = do
  describe "Examples" $ do
    it "withFixedInput \"104\" stdout askGoldbach" $ do
      getOutput (\hOut -> withFixedInput "104" hOut askGoldbach) `shouldReturn` "104=3+101"

    it "withFixedInput \"104\" stdout askGoldbach'" $ do
      getOutput (\hOut -> withFixedInput "104" hOut askGoldbach') `shouldReturn` "104=3+101"

    where askGoldbach = Problem.askGoldbach

spec :: Spec
spec = do
  properties Problem.askGoldbach "askGoldbach"
  properties Problem.askGoldbach' "askGoldbach'"
  examples
  describe "From solutions" $ do
    properties Solution.askGoldbach "askGoldbach"

getOutput :: (Handle -> IO ()) -> IO String
getOutput f = do
  (readEnd, writeEnd) <- createPipe
  f writeEnd
  hClose writeEnd
  s <- hGetLine readEnd
  hClose readEnd
  return s