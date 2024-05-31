{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

{- |
Description: Huffman codes
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P50".
-}
module Problems.P50
  ( huffman
    -- * Supporting functions
    -- | The functions below are not part of the problem.
    -- Instead, they are used to illustrate the use of Huffman coding.
  , countCharacters
  , encodeHuffman
  , decodeHuffman
  , loweralpha
  , ascii
  , text
  ) where

import           Data.List     (group, sort)
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import qualified Solutions.P50 as Solution

{- |
Given a list of characters and their number of occurrences,
construct a list of the characters and their [Huffman encoding](https://brilliant.org/wiki/huffman-encoding/).

In the encoding, @'0'@ and @'1'@ will denote the 0 and 1 bits.

=== Examples

>>> huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
[('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]

The encoding table computed by 'huffman' can be used to compress data:

>>> length $ encodeHuffman (huffman $ countCharacters text) text
3552

Compare this to the length of a fixed-length 5-bit encoding:

>>> length $ encodeHuffman loweralpha text
4375

or the length of the more standard ASCII encoding with 8 bits:

>>> length $ encodeHuffman ascii text
7000

Huffman coding is unambiguous, so we can get back the original text:

>>> let table = huffman $ countCharacters text
>>> let encodedText = encodeHuffman table text
>>> let decodedText = decodeHuffman table encodedText
>>> decodedText == text
True
-}
huffman :: [(Char,Int)] -> [(Char,String)]
huffman = Solution.huffman

-- | Count the number of occurrences of a character in a string.
countCharacters :: String -> [(Char,Int)]
countCharacters s = map (\xs -> (head xs, length xs)) $ group $ sort s

-- | Given an encoding table and a string, encode the string.
--
-- While this is intended for use in illustrating Huffman coding, it is not limited to such.
-- In particular, it can encode with a fixed-length encoding table.
encodeHuffman :: [(Char,String)] -> String -> String
encodeHuffman table string = encodeHuffman' (Map.fromList table) string ""

encodeHuffman' :: Map Char String -> String -> String -> String
encodeHuffman' _ "" encoded = reverse encoded
encodeHuffman' table (c:string) encoded =
  case Map.lookup c table of
    Nothing -> undefined
    Just e  -> encodeHuffman' table string $ reverse e ++ encoded

-- | Given an encoding table and a string, decode the string.
--
-- While this is intended for use in illustrating Huffman coding, it is not limited to such.
-- In particular, it can decode with a fixed-length encoding table.
decodeHuffman :: [(Char,String)] -> String -> String
decodeHuffman table string = decodeHuffman' (Map.fromList $ map (\(x,y) -> (reverse y, x)) table) "" string ""

decodeHuffman' :: Map String Char -> String -> String -> String -> String
decodeHuffman' _ "" "" decoded = reverse decoded
decodeHuffman' table code encoded decoded =
  case Map.lookup code table of
    Nothing -> case encoded of
      (c:encoded') -> decodeHuffman' table (c:code) encoded' decoded
      ""           -> undefined
    Just c -> decodeHuffman' table "" encoded (c:decoded)

-- | Fixed-length encoding of lower case letters and a space using 5 bits.
loweralpha :: [(Char,String)]
loweralpha = [encode c | c <- ' ' : ['a'..'z']]
  where encode c = (c, toBits $ fromEnum c - fromEnum 'a')
        toBits n = getBits n 5

-- | Fixed-length encoding of ASCII characters using 8 bits.
ascii :: [(Char,String)]
ascii = [encode c | c <- ['\0'..'\127']]
  where encode c = (c, toBits $ fromEnum c - fromEnum '\0')
        toBits n = getBits n 8

getBits :: Int -> Int -> String
getBits _ 0 = []
getBits n b = bit : getBits (n `div` 2) (b-1)
  where bit | n `mod` 2 == 1 = '1'
            | otherwise      = '0'

-- | Long text against which various encoding schemes can be tried.
text :: String
text =
  "this is going to be a very long string of text which tries to use all letters " ++
  "of the alphabet  there is no punctuation and no upper case letters because i " ++
  "did not want to write more code to create the encoding table  and it also means " ++
  "it can use fewer bits to encode each letter with a fixed size encoding  " ++
  "using most of the letters means that the five bit encoding is the smallest " ++
  "fixed size encoding for this text  and i actually find it easier to write out " ++
  "this text randomly instead of carefully gathering the letters used by a certain " ++
  "text and getting a smaller fixed size encoding from just those letters  " ++
  "it is trying to be zany for the sake of being zany  being quite long  it is also " ++
  "more convincing that the extra space for the huffman coding table can be worth " ++
  "the extra cost because it is less than the savings we get from encoding a long text " ++
  "with huffman coding"
