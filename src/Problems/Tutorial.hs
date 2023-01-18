{- |
Description: Tutorial for solving problems
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.Tutorial".
-}
module Problems.Tutorial (sumNumbers) where

import qualified Solutions.Tutorial as Solution

{- |

>>> sumNumbers 5 == 1 + 2 + 3 + 4 + 5
True

>>> sumNumbers 100
5050

-}
sumNumbers :: Integer -> Integer
sumNumbers = Solution.sumNumbers
