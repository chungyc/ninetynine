{- |
Description: Group into disjoint subsets
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P27".
-}
module Problems.P27 (disjointGroups) where

import qualified Solutions.P27 as Solution

-- $setup
-- >>> import Data.List (sort)

{- |
In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3, and 4 persons?
Given a list with the sizes of each group and list of items,
write a function to return the list of disjoint groups.

Group sizes are larger than zero, and all items in the list to group are distinct.
Note that we do not want permutations of the group members.
E.g., @[1,2]@ is the same group as @[2,1]@.
However, different groups in the list are considered distinct.
E.g., @[[1,2],[3,4]]@ is considered a different list of groups from @[[3,4],[1,2]]@.

You may find more about this combinatorial problem in a good book on discrete mathematics
under the term [/multinomial coefficients/](https://brilliant.org/wiki/multinomial-coefficients/).

=== Examples

>>> let names = ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
>>> minimum $ map (map sort) $ disjointGroups [2,3,4] names
[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]]
>>> length $ disjointGroups [2,3,4] names
1260
>>> length $ disjointGroups [2,2,5] names
756
-}
disjointGroups :: [Int] -> [a] -> [[[a]]]
disjointGroups = Solution.disjointGroups
