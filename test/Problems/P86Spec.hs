{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P86Spec (spec) where

import           Data.List                 (sortOn)
import           Data.Map.Lazy             ((!))
import qualified Data.Map.Lazy             as Map
import qualified Data.Set                  as Set
import           Problems.Graphs
import           Problems.Graphs.Arbitrary ()
import           Problems.P80
import qualified Problems.P86              as Problem
import qualified Solutions.P86             as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (G -> [(Vertex,Int)]) -> String -> Spec
properties colorGraph name = do
  describe name $ do
    prop "no adjacent vertexes have same color" $
      \g -> colorGraph g `shouldBeVertexColoringFor` g

examples :: Spec
examples = do
  describe "Examples" $ do
    it "colorGraph $ toG $ Paths [[1,2,3,4,5,10,8,6,9,7,2], [1,5], [1,6], [3,8], [4,9], [7,10]]" $
      let g = toG $ Paths [[1,2,3,4,5,10,8,6,9,7,2], [1,5], [1,6], [3,8], [4,9], [7,10]]
      in colorGraph g `shouldBeVertexColoringFor` g

  where colorGraph = Problem.colorGraph

spec :: Spec
spec = parallel $ do
  properties Problem.colorGraph "colorGraph"
  examples
  describe "From solutions" $ do
    properties Solution.colorGraph "colorGraph"

shouldBeVertexColoringFor :: [(Vertex, Int)] -> G -> Property
shouldBeVertexColoringFor cs g =
  let es = Set.toList $ edges g
      coloring = Map.fromList $ cs
      areDistinctColors (Edge (u,v)) = (coloring ! u) /= (coloring ! v)
  in counterexample ("coloring = " ++ show (sortOn fst cs)) $
     conjoin (map (\e -> e `shouldSatisfy` areDistinctColors) es)
