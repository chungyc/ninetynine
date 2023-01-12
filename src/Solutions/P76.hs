{- |
Description: Either monad
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P76" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P76 (eitherGoldbach) where

import           Solutions.P40 (goldbach)
import           Text.Read     (readMaybe)

-- | Rewrite of 'Problems.P75.maybeGoldbach' to return an 'Either' value.
eitherGoldbach :: String -> Either String (Integer, (Integer, Integer))
eitherGoldbach s = do
  n <- readNumber $ readMaybe s
  if n > 2 then Right () else Left "not greater than 2"
  if even n then Right () else Left "not an even number"
  return $ (n, goldbach n)

-- From the result of 'readMaybe', return either the number or a @"not a number"@ error.
-- 'Text.Read.readEither' could have been used,
-- except we want to use a specific error string instead of a library-defined one.
readNumber :: Maybe Integer -> Either String Integer
readNumber Nothing  = Left "not a number"
readNumber (Just n) = Right n
