module Problems.P66Spec (spec) where

import           Data.List                      (sort)
import           Problems.BinaryTrees
import           Problems.BinaryTrees.Arbitrary ()
import           Problems.P65
import qualified Problems.P66                   as Problem
import qualified Solutions.P66                  as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Tree Char -> Tree (Char, (Int, Int))) -> String -> Spec
properties layoutCompact name = do
  describe name $ do
    prop "is compact" $
      \t -> layoutCompact t `shouldSatisfy` isCompact

    prop "does not overlap subtrees" $
      \t -> layoutCompact t `shouldSatisfy` not . overlaps

    prop "has symmetrical horizontal distances from parent node to child nodes" $
      \t -> layoutCompact t `shouldSatisfy` isSymmetric . toDistanceTree

    prop "has leftmost node at correct horizontal position" $
      \t -> t /= Empty ==>
            layoutCompact t `shouldSatisfy` (==) (Just 1) . minPosX

    prop "vertical location is depth" $
      \t -> layoutCompact t `shouldSatisfy` verticalIsDepth

    prop "keeps original values" $
      \t -> layoutCompact t `shouldSatisfy` hasSameValuesAs t

examples :: Spec
examples = do
  describe "Examples" $ do
    it "layoutCompact tree65" $ do
      layoutCompact tree65 `shouldBe`
        Branch ('n',(5,1))
        (Branch ('k',(3,2))
         (Branch ('c',(2,3))
          (Branch ('a',(1,4)) Empty Empty)
          (Branch ('e',(3,4))
           (Branch ('d',(2,5)) Empty Empty)
           (Branch ('g',(4,5)) Empty Empty)))
          (Branch ('m',(4,3)) Empty Empty))
        (Branch ('u',(7,2))
         (Branch ('p',(6,3)) Empty
          (Branch ('q',(7,4)) Empty Empty))
          Empty)

  where layoutCompact = Problem.layoutCompact

spec :: Spec
spec = parallel $ do
  properties Problem.layoutCompact "layoutCompact"
  examples
  describe "From solutions" $ do
    properties Solution.layoutCompact "layoutCompact"

isCompact :: Tree (a, (Int, Int)) -> Bool
isCompact Empty = True
isCompact (Branch _ _ Empty) = True
isCompact (Branch _ Empty _) = True
isCompact (Branch _ l r) = cannotBeCloser && isCompact l && isCompact r
  where cannotBeCloser = overlapping (shift (bounds l) 1) (shift (bounds r) (-1))
        shift [] _         = []
        shift ((u,v):bs) n = (u+n,v+n) : shift bs n

overlaps :: Tree (a, (Int, Int)) -> Bool
overlaps Empty = False
overlaps (Branch _ l r) = overlapping bl br || overlaps l || overlaps r
  where bl = bounds l
        br = bounds r

-- | Whether range of positions at each level of subtrees can overlap.
overlapping :: [(Int,Int)] -> [(Int,Int)] -> Bool
overlapping [] []                    = False
overlapping [] _                     = False
overlapping _ []                     = False
overlapping ((u,v):bs) ((u',v'):bs') = outOfOrder || coincides || overlapping bs bs'
  where outOfOrder = sort [u,v,u',v'] /= [u,v,u',v']
        coincides = v == u'

-- | The leftmost and rightmost positions at each level of a tree.
bounds :: Tree (a, (Int, Int)) -> [(Int,Int)]
bounds Empty = []
bounds (Branch (_, (b, _)) l r) = (b,b) : merge (bounds l) (bounds r)
  where merge [] []                        = []
        merge [] bs                        = bs
        merge bs []                        = bs
        merge ((bl,_) : bs) ((_,br) : bs') = (bl, br) : merge bs bs'

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
