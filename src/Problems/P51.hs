{- |
Description: Error correction codes
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P51".
-}
module Problems.P51 (corrupt, errorCorrectingEncode, errorCorrectingDecode) where

import qualified Solutions.P51 as Solution
import           System.Random

{- |
Flip a given number of boolean values in the boolean list randomly.

=== Examples

>>> corrupt (mkStdGen 111) 2 [False, True, True, False, True]
[False,False,True,True,False]
-}
corrupt :: RandomGen g => g -> Int -> [Bool] -> [Bool]
corrupt = Solution.corrupt

{- |
Construct an error-correcting encoding of the given Boolean list.

The encoding must be able to correct at least one error.
Consider using a [repetition code](https://en.wikipedia.org/wiki/Repetition_code) of length 3.
-}
errorCorrectingEncode :: [Bool] -> [Bool]
errorCorrectingEncode = Solution.errorCorrectingEncode

{- |
The inverse of 'errorCorrectingEncode'.
Recover the original Boolean list from its encoding.
There could be a single error in the encoding.

=== Examples

>>> errorCorrectingDecode . errorCorrectingEncode $ [False, False, True, False]
[False,False,True,False]

>>> let e = errorCorrectingEncode [True, False, False, True, False]
>>> let e' = corrupt (mkStdGen 111) 1 e
>>> errorCorrectingDecode e'
[True,False,False,True,False]
-}
errorCorrectingDecode :: [Bool] -> [Bool]
errorCorrectingDecode = Solution.errorCorrectingDecode
