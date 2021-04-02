{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P65Spec (spec) where

import           Problems.BinaryTrees
import           Problems.BinaryTrees.Arbitrary ()
import qualified Problems.P65                   as Problem
import qualified Solutions.P65                  as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Tree Char -> Tree (Char, (Int, Int))) -> String -> Spec
properties layoutLevelConstant name = do
  describe name $ do
    prop "has symmetrical horizontal distances from parent node to child nodes" $
      \t -> layoutLevelConstant t `shouldSatisfy` isSymmetric . toDistanceTree

    prop "has distances halve with each level" $
      \t -> counterexample (show $ toDistanceTree $ layoutLevelConstant t) $
            layoutLevelConstant t `shouldSatisfy` isHalved . toDistanceTree

    prop "has mininum horizontal distance of 1 between parent and child" $
      \t -> treeHeight t > 1 ==>
            layoutLevelConstant t `shouldSatisfy` (==) (Just 1) . minDistance . toDistanceTree

    prop "has leftmost node at correct horizontal position" $
      \t -> t /= Empty ==>
            layoutLevelConstant t `shouldSatisfy` (==) (Just 1) . minPosX

    prop "vertical location is depth" $
      \t -> layoutLevelConstant t `shouldSatisfy` verticalIsDepth

    prop "keeps original values" $
      \t -> layoutLevelConstant t `shouldSatisfy` hasSameValuesAs t

examples :: Spec
examples = do
  describe "Examples" $ do
    it "layoutLevelConstant tree65" $ do
      layoutLevelConstant tree65 `shouldBe`
        Branch ('n',(15,1)) (Branch ('k',(7,2))
                             (Branch ('c',(3,3))
                              (Branch ('a',(1,4)) Empty Empty)
                              (Branch ('e',(5,4))
                               (Branch ('d',(4,5)) Empty Empty)
                               (Branch ('g',(6,5)) Empty Empty)))
                             (Branch ('m',(11,3)) Empty Empty))
                            (Branch ('u',(23,2))
                             (Branch ('p',(19,3))
                              Empty
                              (Branch ('q',(21,4)) Empty Empty)) Empty)

  where layoutLevelConstant = Problem.layoutLevelConstant
        tree65 = Problem.tree65

spec :: Spec
spec = parallel $ do
  properties Problem.layoutLevelConstant "layoutLevelConstant"
  examples
  describe "From solutions" $ do
    properties Solution.layoutLevelConstant "layoutLevelConstant"

toDistanceTree :: Tree (a, (Int, Int)) -> Tree (Maybe Int, Maybe Int)
toDistanceTree Empty = Empty
toDistanceTree (Branch (_, (x,_)) l r) = Branch (distance l, distance r) l' r'
  where distance Empty                    = Nothing
        distance (Branch (_, (x',_)) _ _) = Just $ x' - x
        l' = toDistanceTree l
        r' = toDistanceTree r

-- | Whether each node has symmetrical horizontal distances between each child,
-- with left child being left of right child.  Not to be confused with symmetric trees.
isSymmetric :: Tree (Maybe Int, Maybe Int) -> Bool
isSymmetric Empty = True
isSymmetric (Branch (ld, rd) l r) = comp ld rd && isSymmetric l && isSymmetric r
  where comp Nothing _         = True
        comp _ Nothing         = True
        comp (Just x) (Just y) = x < y && (-x) == y

isHalved :: Tree (Maybe Int, Maybe Int) -> Bool
isHalved Empty = True
isHalved (Branch (ld, rd) l r) = ld `halvesTo` dist l && rd `halvesTo` dist r
  where halvesTo Nothing  (Just _)  = False
        halvesTo (Just d) (Just d') = abs d == 2 * (abs d')
        halvesTo _        _         = True
        dist (Branch (_, Just d) _ _) = Just d
        dist (Branch (Just d, _) _ _) = Just d
        dist _                        = Nothing

minDistance :: Tree (Maybe Int, Maybe Int) -> Maybe Int
minDistance Empty = Nothing
minDistance (Branch (ld, rd) l r) = ld `mindist` rd `mindist` lmin `mindist` rmin
  where mindist Nothing Nothing   = Nothing
        mindist Nothing (Just n)  = Just $ abs n
        mindist (Just n) Nothing  = Just $ abs n
        mindist (Just x) (Just y) = Just $ min (abs x) (abs y)
        lmin = minDistance l
        rmin = minDistance r

minPosX :: Tree (a, (Int, Int)) -> Maybe Int
minPosX Empty                    = Nothing
minPosX (Branch (_, (x, _)) l r) = Just $ minimum $ [x] ++ lmin ++ rmin
  where lmin = minX $ minPosX l
        rmin = minX $ minPosX r
        minX Nothing  = []
        minX (Just n) = [n]

verticalIsDepth :: Tree (a, (Int, Int)) -> Bool
verticalIsDepth t = verticalIsDepth' t 1

verticalIsDepth' :: Tree (a, (Int, Int)) -> Int -> Bool
verticalIsDepth' Empty _ = True
verticalIsDepth' (Branch (_, (_, y)) l r) d =
  d == y && verticalIsDepth' l (d+1) && verticalIsDepth' r (d+1)

hasSameValuesAs :: Eq a => Tree a -> Tree (a, (Int, Int)) -> Bool
hasSameValuesAs Empty Empty = True
hasSameValuesAs (Branch x l r) (Branch (x', _) l' r') =
  x == x' && hasSameValuesAs l l' && hasSameValuesAs r r'
hasSameValuesAs _ _ = False
