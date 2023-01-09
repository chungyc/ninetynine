{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P47Spec (spec) where

import qualified Problems.P47          as Problem
import qualified Solutions.P47         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (([(Int,Int)] -> Bool -> Bool -> Bool), (Bool -> Bool -> Bool) -> [(Int,Int)])
           -> (String, String)
           -> Spec
properties (evaluateCircuit, buildCircuit) (nameEvaluateCircuit, nameBuildCircuit) = do
  describe nameEvaluateCircuit $ do
    prop "feeds outputs from indexed input gates into a gate" $
      \(Circuit c) -> \x -> \y ->
        let eval (-1) = x
            eval (-2) = y
            eval i    = case c !! (i-1) of (j,k) -> eval j `nand` eval k
        in evaluateCircuit c x y `shouldBe` eval (length c)

    prop "has output from last gate" $
      \(Circuit c) -> \x -> \y ->
        let (l,r) = last c
            eval (-1) = x
            eval (-2) = y
            eval i    = evaluateCircuit (trim c i) x y
        in evaluateCircuit c x y `shouldBe` nand (eval l) (eval r)

    prop "is inverse of buildCircuit" $
      \f -> \x -> \y ->
        evaluateCircuit (buildCircuit $ applyFun2 f) x y `shouldBe` applyFun2 f x y

  describe nameBuildCircuit $ do
    prop "builds logic circuits for binary boolean functions" $
      \f -> buildCircuit (applyFun2 f)
            `shouldSatisfy` (==) (table $ applyFun2 f) . table . evaluateCircuit

    prop "is almost inverse of evaluateCircuit" $
      \(Circuit c) -> \x -> \y ->
        evaluateCircuit (buildCircuit $ evaluateCircuit c) x y
        `shouldBe` evaluateCircuit c x y
        -- It should build a circuit with equivalent output,
        -- but it does not have to be the same circuit.

examples :: Spec
examples = describe "Examples" $ do
  it "evaluateCircuit [(-1,-2)] True True" $ do
    evaluateCircuit [(-1,-2)] True True `shouldBe` False

  it "evaluateCircuit [(-1,-2)] True False" $ do
    evaluateCircuit [(-1,-2)] True False `shouldBe` True

  it "evaluateCircuit [(-1,-2), (1,1)] True True" $ do
    evaluateCircuit [(-1,-2), (1,1)] True True `shouldBe` True

  it "evaluateCircuit [(-1,-2), (1,1)] True False" $ do
    evaluateCircuit [(-1,-2), (1,1)] True False `shouldBe` False

  it "evaluateCircuit [(-1,-1),(-2,-2),(1,2)] True False" $ do
    evaluateCircuit [(-1,-1),(-2,-2),(1,2)] True False `shouldBe` True

  it "evaluateCircuit [(-1,-1),(-2,-2),(1,2)] False False" $ do
    evaluateCircuit [(-1,-1),(-2,-2),(1,2)] False False `shouldBe` False

  it "evaluateCircuit (buildCircuit (&&)) False False" $ do
    evaluateCircuit (buildCircuit (&&)) False False `shouldBe` False

  it "evaluateCircuit (buildCircuit (&&)) False True" $ do
    evaluateCircuit (buildCircuit (&&)) False True `shouldBe` False

  it "evaluateCircuit (buildCircuit (&&)) True False" $ do
    evaluateCircuit (buildCircuit (&&)) True False `shouldBe` False

  it "evaluateCircuit (buildCircuit (&&)) True True" $ do
    evaluateCircuit (buildCircuit (&&)) True True `shouldBe` True

  where evaluateCircuit = Problem.evaluateCircuit
        buildCircuit = Problem.buildCircuit

spec :: Spec
spec = parallel $ do
  properties
    (Problem.evaluateCircuit, Problem.buildCircuit)
    ("evaluateCircuit", "buildCircuit")
  examples
  describe "From solutions" $ do
    properties
      (Solution.evaluateCircuit, Solution.buildCircuit)
      ("evaluateCircuit", "buildCircuit")

nand :: Bool -> Bool -> Bool
nand x y = not $ x && y

-- | Trim circuit so that gate at given index is the output gate.
--
-- The rules ensure that the truncated logic circuit is still a valid circuit.
trim :: [(Int,Int)] -> Int -> [(Int,Int)]
trim c i = take i c

-- | Truth table for given boolean function.
table :: (Bool -> Bool -> Bool) -> (Bool,Bool,Bool,Bool)
table f = (f False False, f False True, f True False, f True True)

newtype Circuit = Circuit [(Int,Int)] deriving Show

instance Arbitrary Circuit where
  arbitrary = sized gen
    where gen 0 = gen 1

          gen 1 = do
            i <- oneof vars
            j <- oneof vars
            return $ Circuit [(i,j)]
              where vars = [return $ -1, return $ -2]

          gen n = do
            i <- frequency inputs
            j <- frequency inputs
            Circuit c' <- gen (n-1)
            return $ Circuit $ c' ++ [(i,j)]
              where inputs = [ (1, return $ -1)
                             , (1, return $ -2)
                             , (n, chooseInt (1,n-1))
                             ]

  shrink (Circuit [])  = []
  shrink (Circuit [_]) = []
  shrink (Circuit c)   = [Circuit $ init c]
