{- |
Description: 'removeAt'

Some solutions to "Problems.P20" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P20 (removeAt) where

-- | Remove the @k@th element from a list.
-- Return the element removed and the residue list.
removeAt :: Int -> [a] -> (a,[a])
removeAt n xs = (e, front ++ back)
  where front = take (n-1) xs
        (e : back) = drop (n-1) xs
