module Problems.P73Spec (spec) where

import           Control.Monad
import           Data.List                        (intercalate)
import           Problems.MultiwayTrees
import           Problems.MultiwayTrees.Arbitrary ()
import qualified Problems.P73                     as Problem
import qualified Solutions.P73                    as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (MultiwayTree Char -> String, String -> MultiwayTree Char) -> (String, String) -> Spec
properties (treeToSexp, sexpToTree) (nameTreeToSexp, nameSexpToTree) = do
  describe nameTreeToSexp $ do
    prop "is list of node value and subtrees" $
      \(Tree t@(MultiwayTree x ts)) ->
        not (null ts) ==>
        treeToSexp t `shouldBe` "(" ++ (intercalate " " $ [x] : map treeToSexp ts) ++ ")"

    prop "is bare for tree with no children" $
      forAll (elements ['a'..'z']) $ \x ->
      treeToSexp (MultiwayTree x []) `shouldBe` [x]

    prop "is inverse of sexpToTree" $
      \(SexpStr s) -> (treeToSexp . sexpToTree) s `shouldBe` s

  describe nameSexpToTree $ do
    prop "is inverse of treeToSexp" $
      \(Tree t) -> (sexpToTree . treeToSexp) t `shouldBe` t

examples :: Spec
examples = do
  describe "Examples" $ do
    it "treeToSexp multitree1" $ do
      treeToSexp multitree1 `shouldBe` "a"

    it "treeToSexp multitree2" $ do
      treeToSexp multitree2 `shouldBe` "(a b)"

    it "treeToSexp multitree3" $ do
      treeToSexp multitree3 `shouldBe` "(a (b c))"

    it "treeToSexp multitree4" $ do
      treeToSexp multitree4 `shouldBe` "(b d e)"

    it "treeToSexp multitree5" $ do
      treeToSexp multitree5 `shouldBe` "(a (f g) c (b d e))"

    it "sexpToTree \"(a (f g) c (b d e))\"" $ do
      sexpToTree "(a (f g) c (b d e))" `shouldBe` multitree5

  where treeToSexp = Problem.treeToSexp
        sexpToTree = Problem.sexpToTree

spec :: Spec
spec = do
  properties (Problem.treeToSexp, Problem.sexpToTree) ("treeToSexp", "sexpToTree")
  examples
  describe "From solutions" $ do
    properties (Solution.treeToSexp, Solution.sexpToTree) ("treeToSexp", "sexpToTree")

newtype Tree = Tree (MultiwayTree Char)
  deriving (Eq, Show)

instance Arbitrary Tree where
  arbitrary = do
    t <- arbitrary
    return $ Tree $ exclude t

-- | Exclude special characters for an s-expression.
exclude :: MultiwayTree Char -> MultiwayTree Char
exclude (MultiwayTree '(' ts) = MultiwayTree '.' $ map exclude ts
exclude (MultiwayTree ')' ts) = MultiwayTree '.' $ map exclude ts
exclude (MultiwayTree ' ' ts) = MultiwayTree '.' $ map exclude ts
exclude (MultiwayTree x   ts) = MultiwayTree x   $ map exclude ts

-- | Generates arbitrary s-expressions representing multiway trees according to the rules.
newtype SexpStr = SexpStr String
  deriving (Eq, Show)

instance Arbitrary SexpStr where
  arbitrary = sized $ genSexpStr

genSexpStr :: Int -> Gen SexpStr
genSexpStr 0 = do
  (Atom c) <- arbitrary
  return $ SexpStr [c]
genSexpStr 1 = do
  (Atom c) <- arbitrary
  return $ SexpStr [c]
genSexpStr n = do
  (Atom c) <- arbitrary
  m <- chooseInt (1, n-1)
  let s = (n `div` m) - 1
  let t = liftM sexpStr <$> resize m $ arbitrary
  let ts = resize s $ listOf1 t
  let list = liftM ((:) [c]) ts
  let substr = liftM surround $ liftM (intercalate " ") list
  frequency [ (1, return $ SexpStr [c]), (n-1, liftM SexpStr substr) ]
    where surround s = "(" ++ s ++ ")"

sexpStr :: SexpStr -> String
sexpStr (SexpStr s) = s

newtype Atom = Atom Char
  deriving (Eq, Show)

instance Arbitrary Atom where
  arbitrary = do
    c <- elements ['a'..'z']
    return $ Atom c
