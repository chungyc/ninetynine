{- |
Description: 'disjointGroups'

Some solutions to "Problems.P27" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P27 (disjointGroups) where

{- |
Group the elements of a set into disjoint subsets.

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
-}
disjointGroups :: [Int] -> [a] -> [[[a]]]
disjointGroups [] []     = [[]]
disjointGroups [] xs     = [[xs]]  -- implicitly another group if group sizes do not sum to length
disjointGroups (n:ns) xs = concat $ map (\(g,xs') -> map (g:) $ disjointGroups ns xs') $ extract n xs

extract :: Int -> [a] -> [([a], [a])]
extract n xs
  | length xs <= n = [(xs,[])]  -- last group may be smaller than given size
  | otherwise      = extract' n ([],xs)

extract' :: Int -> ([a], [a]) -> [([a],[a])]
extract' 0 r        = [r]
extract' _ (_, [])  = []
extract' n (g,x:xs) = (map (\(g',xs') -> (g',x:xs')) $ extract' n (g,xs)) ++ extract' (n-1) (x:g,xs)
