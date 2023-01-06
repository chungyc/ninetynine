{- |
Description: Gray codes
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P49" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P49 (gray) where

-- | Returns the n-bit Gray code.
gray :: Int -> [String]
gray 0 = []
gray 1 = ["0", "1"]
gray n
  | n > 1     = map ('0':) g ++ map ('1':) (reverse g)
  | otherwise = undefined
  where g = gray (n-1)
