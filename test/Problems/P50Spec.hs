{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P50Spec (spec) where

import           Data.List             (group, isPrefixOf, nub, sort)
import           Data.Map.Lazy         (Map, (!))
import qualified Data.Map.Lazy         as Map
import           Data.Maybe            (fromJust)
import           Problems.P50          (countCharacters, decodeHuffman,
                                        encodeHuffman, loweralpha, text)
import qualified Problems.P50          as Problem
import qualified Solutions.P50         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([(Char,Int)] -> [(Char,String)]) -> String -> Spec
properties huffman name = describe name $ do
  prop "has code for all characters" $ \(Counts cs) ->
    huffman cs `shouldSatisfy`
    (==) (sort $ map fst cs) . sort . map fst

  prop "each character has one code" $ \(Counts cs) ->
    huffman cs `shouldSatisfy`
    all (==1) . map length . group . sort . map fst

  prop "no code is empty" $ \(Counts cs) ->
    huffman cs `shouldSatisfy`
    all (not . null) . map snd

  prop "code is not longer than code for more frequent character" $ \(Counts cs) ->
    length cs > 1 ==>
    forAll (elements $ map fst cs) $ \x ->
    forAll (elements $ map fst cs) $ \y ->
    retrieve x cs < retrieve y cs ==>
    huffman cs `shouldSatisfy`
    \e -> length (retrieve x e) >= length (retrieve y e)

  prop "has no code which is a prefix of another" $ \(Counts cs) ->
    length cs > 1 ==>
    forAll (elements $ map fst cs) $ \x ->
    forAll (elements $ map fst cs) $ \y ->
    x /= y ==>
    huffman cs `shouldSatisfy`
    \e -> not (retrieve x e `isPrefixOf` retrieve y e)

  prop "is unambiguous encoding" $ \s ->
    let counts = countCharacters s
        table = huffman counts
    in counterexample ("counts = " ++ show counts) $
       counterexample ("encoding = " ++ show table) $
       (decodeHuffman table . encodeHuffman table) s `shouldBe` s

  prop "does not leave shorter codes unused" $ \(Counts cs) ->
    length cs > 1 ==>
    huffman cs `shouldSatisfy` isCompact . toTree

  prop "does not have child trees that are lighter than grandchild trees" $ \(Counts cs) ->
    length cs > 1 ==>
    huffman cs `shouldSatisfy` isGreedy (Map.fromList cs) . toTree

  where retrieve x xs = fromJust $ lookup x xs

examples :: Spec
examples = describe "Examples" $ do
  it "huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]" $
    let counts = [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
        string = concat $ map (\(c,n) -> replicate n c) counts
        t = huffman counts
    in do
      length (encodeHuffman t string) `shouldBe` 224
      (decodeHuffman t . encodeHuffman t) string `shouldBe` string
      -- There can be more than one Huffman encoding which are valid for
      -- the same character frequencies, but they should all result
      -- in an encoded text with the same length, and they should
      -- all be unambiguously decodeable back to the same text.

  it "length $ encodeHuffman (huffman $ countCharacters text) text" $
    length (encodeHuffman (huffman $ countCharacters text) text) `shouldBe` 3552

  it "length $ encodeHuffman loweralpha text" $ do
    length (encodeHuffman loweralpha text) `shouldBe` 4375

  it "decodeHuffman (huffman ...) $ encodeHuffman (huffman ...) text" $
    let table = huffman $ countCharacters text
        encodedText = encodeHuffman table text
        decodedText = decodeHuffman table encodedText
    in decodedText `shouldBe` text

  where huffman = Problem.huffman

spec :: Spec
spec = parallel $ do
  properties Problem.huffman "huffman"
  examples
  describe "From solutions" $ do
    properties Solution.huffman "huffman"

-- | Left is 0, right is 1.
data HuffmanTree = Branch HuffmanTree HuffmanTree | Leaf (Maybe Char)
  deriving Show

-- | Builds up a Huffman tree based on the given encoding table.
toTree :: [(Char,String)] -> HuffmanTree
toTree table = foldl incorporate (Leaf Nothing) table

-- | Incorporate a character and its encoding into a Huffman tree
incorporate :: HuffmanTree -> (Char,String) -> HuffmanTree
incorporate (Leaf Nothing) (c, "") = Leaf $ Just c
incorporate (Leaf _) (_, "") = error "duplicate code"
incorporate (Leaf Nothing) (c, '0':code) =
  Branch (incorporate (Leaf Nothing) (c, code)) (Leaf Nothing)
incorporate (Leaf Nothing) (c, '1':code) =
  Branch (Leaf Nothing) (incorporate (Leaf Nothing) (c, code))
incorporate (Branch l r) (c, '0':code) = Branch (incorporate l (c, code)) r
incorporate (Branch l r) (c, '1':code) = Branch l (incorporate r (c, code))
incorporate _ _ = error "invalid encoding"

-- | Confirms that the Huffman tree does not leave any shorter codes unused.
isCompact :: HuffmanTree -> Bool
isCompact (Leaf Nothing) = False
isCompact (Leaf _)       = True
isCompact (Branch l r)   = isCompact l && isCompact r

-- | Confirm whether the Huffman tree was built up consistent with a greedy approach.
-- I.e., for a particular node in the tree, the weight for each individual child subtree
-- should not be lighter than any of the grandchild subtrees.
isGreedy :: Map Char Int -> HuffmanTree -> Bool
isGreedy _ (Leaf _)     = True
isGreedy m (Branch l r) = all (\w -> all (\w' -> w >= w') grandchildWeights) childWeights
  where weight (Leaf (Just c)) = m ! c
        weight (Branch l' r')  = weight l' + weight r'
        weight _               = undefined
        childWeights = [weight l, weight r]
        grandchildWeights = subtreeWeights l ++ subtreeWeights r
        subtreeWeights (Leaf _)       = []
        subtreeWeights (Branch l' r') = [weight l', weight r']

-- | Arbitrary list of distinct characters and associated counts.
newtype Counts = Counts [(Char,Int)] deriving Show

instance Arbitrary Counts where
  arbitrary = sized $ \n ->
    combine <$>
    vectorOf n arbitraryPrintableChar <*>
    infiniteListOf (arbitrarySizedNatural `suchThat` (>1))
    where combine xs ys = Counts $ zip (nub xs) ys

  shrink (Counts cs) = map Counts $ shrinkList shrink cs
