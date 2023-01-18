{- |
Description: Random permutation of a list
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P25" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P25 (randomPermute) where

-- The implementation for problem 23 also happens to solve this problem.
-- Note that this does *not* import Problems.P23,
-- since the problem does not specify that the order is random.
import           Solutions.P23
import           System.Random

-- | Generate a random permutation of the elements of a list.
randomPermute :: RandomGen g => [a] -> g -> ([a], g)
randomPermute xs = randomSelect xs (length xs)
