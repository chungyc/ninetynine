{- |
Description: Either monad
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P76".
-}
module Problems.P76 (eitherGoldbach) where

import qualified Solutions.P76 as Solution

{- |
In "Problems.P75", 'Problems.P75.maybeGoldbach' returned 'Nothing' when there is an error.
However, this revealed nothing about why there is an error.

'Either' is a data type which can hold either of two data types,
which can be used to store either an error or a correct value when there is no error.
By convention when 'Either' is used this way, the 'Left' constructor is used for errors
and the 'Right' constructor is used for correct values.  'Either' is also a monad.

Rewrite 'Problems.P75.maybeGoldbach' to return an 'Either' value,
using one of the following strings when there is an error with their obvious meanings:

* @"not a number"@
* @"not greater than 2"@
* @"not an even number"@

=== Examples

>>> eitherGoldbach "104"
Right (104,(3,101))

>>> eitherGoldbach "this is not a number"
Left "not a number"

>>> eitherGoldbach "2"
Left "not greater than 2"

>>> eitherGoldbach "101"
Left "not an even number"
-}
eitherGoldbach :: String -> Either String (Int, (Int,Int))
eitherGoldbach = Solution.eitherGoldbach
