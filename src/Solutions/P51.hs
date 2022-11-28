{- |
Description: Error correction codes
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P51" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P51 (corrupt, errorCorrectingEncode, errorCorrectingDecode) where

import           Data.List     (nub, sort)
import           System.Random

{- |
Flip a given number of boolean values in the boolean list randomly.
-}
corrupt :: RandomGen g => g -> Int -> [Bool] -> [Bool]
corrupt gen n s = corrupt' 0 positions s
  where n' = min n $ length s
        positions = sort $ take n' $ nub $ randomRs (0, (length s) - 1) gen

corrupt' :: Int -> [Int] -> [Bool] -> [Bool]
corrupt' _ _ [] = []
corrupt' _ [] s = s
corrupt' offset ps@(n:ns) (c:cs)
  | n == offset = (not c) : (corrupt' (offset+1) ns cs)
  | otherwise   = c : (corrupt' (offset+1) ps cs)

{- |
Construct an error-correcting encoding of the given Boolean list.

The encoding must be able to correct at least one error.
Consider using a [repetition code](https://en.wikipedia.org/wiki/Repetition_code) of length 3.
-}
errorCorrectingEncode :: [Bool] -> [Bool]
errorCorrectingEncode [] = []
errorCorrectingEncode (False : xs) = False : False : False : errorCorrectingEncode xs
errorCorrectingEncode (True : xs) = True : True : True : errorCorrectingEncode xs

{- |
The inverse of 'errorCorrectingEncode'.
Recover the original Boolean list from its encoding.
There could be an error in the encoding.
-}
errorCorrectingDecode :: [Bool] -> [Bool]
errorCorrectingDecode []        = []
errorCorrectingDecode (a:b:c:l) = vote (a,b,c) : errorCorrectingDecode l
errorCorrectingDecode (a:b:[])  = [vote (a,b,False)]  -- arbitrarily bias to False when bit missing
errorCorrectingDecode (a:[])    = [a]

vote :: (Bool, Bool, Bool) -> Bool
vote (a, b, c)
  | a' + b' + c' > 1 = True
  | otherwise        = False
  where a' = toCount a
        b' = toCount b
        c' = toCount c
        toCount False = 0 :: Int
        toCount True  = 1 :: Int
