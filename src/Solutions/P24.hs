{- |
Description: 'randomDraw'

Some solutions to "Problems.P24" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P24 (randomDraw) where

import           Problems.P23
import           System.Random

-- | Lotto: Draw \(n\) different random numbers from the set \( \{ k \,|\, 1 \leq k \leq m \} \).
randomDraw :: RandomGen g => Int -> Int -> g -> ([Int], g)
randomDraw n m g = randomSelect [1..m] n g
