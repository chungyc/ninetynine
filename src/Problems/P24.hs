{- |
Description: Draw random numbers
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P24".
-}
module Problems.P24 (randomDraw) where

import qualified Solutions.P24 as Solution
import           System.Random

-- | Draw \(n\) different random numbers from the set \( \{ k \,|\, 1 \leq k \leq m \} \).
--
-- === Examples
--
-- >>> fst $ randomDraw 6 49 $ mkStdGen 111
-- [40,13,25,27,26,6]
--
-- >>> take 5 $ unfoldr (Just . randomDraw 3 100) $ mkStdGen 111
-- [[11,19,76],[63,49,10],[75,42,12],[20,48,78],[40,94,86]]
--
-- >>> fst . randomDraw 6 49 <$> newStdGen
-- [17,7,1,18,13,3]
randomDraw :: RandomGen g => Int -> Int -> g -> ([Int], g)
randomDraw = Solution.randomDraw
