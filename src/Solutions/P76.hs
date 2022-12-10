{- |
Description: Either monads
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P76" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P76 (eitherGoldbach) where

import           Solutions.P40 (goldbach)
import           Text.Read     (readMaybe)

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
-}
eitherGoldbach :: String -> Either String (Int, (Int,Int))
eitherGoldbach s = do
  n <- readNumber $ readMaybe s
  if n > 2 then Right () else Left "not greater than 2"
  if even n then Right () else Left "not an even number"
  return $ (n, goldbach n)

-- From the result of 'readMaybe', return either the number or a @"not a number"@ error.
-- 'Text.Read.readEither' could have been used,
-- except we want to use a specific error string instead of a library-defined one.
readNumber :: Maybe Int -> Either String Int
readNumber Nothing  = Left "not a number"
readNumber (Just n) = Right n
