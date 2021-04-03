{- |
Description: Draw random numbers
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P24" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P24 (randomDraw) where

import           Problems.P23
import           System.Random

-- | Draw \(n\) different random numbers from the set \( \{ k \,|\, 1 \leq k \leq m \} \).
randomDraw :: RandomGen g => Int -> Int -> g -> ([Int], g)
randomDraw n m g = randomSelect [1..m] n g
