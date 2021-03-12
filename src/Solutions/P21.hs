{- |
Description: 'insertAt'

Some solutions to "Problems.P21" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P21 (insertAt) where

-- | Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = front ++ [x] ++ back
  where front = take (n-1) xs
        back = drop (n-1) xs
