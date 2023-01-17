{- |
Description: Select random elements from a list
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P23" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P23 (randomSelect) where

import           Data.Set      (Set)
import qualified Data.Set      as Set
import           System.Random

-- | Extract a given number of randomly selected elements from a list.
--
-- Also return a new random number generator so that callers
-- can avoid reusing a sequence of random numbers.
randomSelect :: RandomGen g => [a] -> Int -> g -> ([a], g)
randomSelect xs n = select (Set.fromList $ zipWith (curry Indexed) [1..] xs) n []

-- | For storing an arbitrary type in 'Set'.
newtype Indexed a = Indexed (Int, a)

instance Eq (Indexed a) where
  (==) (Indexed (i, _)) (Indexed (j, _)) = i == j

instance Ord (Indexed a) where
  compare (Indexed (i,_)) (Indexed (j,_)) = compare i j

toValue :: Indexed a -> a
toValue (Indexed (_,x)) = x

select :: RandomGen g => Set (Indexed a) -> Int -> [a] -> g -> ([a], g)
select _ 0 l g = (l, g)
select xs n l g
  | Set.null xs = (l, g)
  | otherwise   = select xs' (n-1) (toValue x : l) g'
  where (i, g') = randomR (0, Set.size xs - 1) g
        x = Set.elemAt i xs
        xs' = Set.deleteAt i xs
