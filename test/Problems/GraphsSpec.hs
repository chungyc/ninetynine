module Problems.GraphsSpec (spec) where

import           Problems.Graphs
import           Problems.Graphs.Arbitrary ()
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = do
  prop "sets are vertexes and edges" $
    \g -> sets (g :: G) `shouldBe` (vertexes g, edges g)
