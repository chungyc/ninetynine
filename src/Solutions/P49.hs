{- |
Description: Gray codes
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P49" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P49 (gray) where

-- | An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules.
-- For example,
--
-- >>> gray 1  -- 1-bit gray code
-- ["0","1"]
--
-- >>> gray 2  -- 2-bit gray code
-- ["00","01","11","10"]
--
-- >>> gray 3  -- 3-bit gray code
-- ["000","001","011","010","110","111","101","100"]
--
-- Infer the construction rules and write a function returning the n-bit Gray code.
gray :: Int -> [String]
gray 0 = []
gray 1 = ["0", "1"]
gray n
  | n > 1     = map ('0':) g ++ map ('1':) (reverse g)
  | otherwise = undefined
  where g = gray (n-1)
