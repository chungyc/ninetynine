{- |
Description: Maybe monad
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P75" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P75 (maybeGoldbach) where

import           Control.Monad (guard)
import           Solutions.P40 (goldbach)
import           Text.Read     (readMaybe)

-- | Implementation of 'Problems.P75.maybeGoldbach'' with do notation.
maybeGoldbach :: String -> Maybe (Integer, (Integer, Integer))
maybeGoldbach s = do
  n <- readMaybe s
  guard $ n > 2
  guard $ even n
  return (n, goldbach n)
