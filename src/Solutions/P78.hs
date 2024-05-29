{- |
Description: Collatz conjecture
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P78" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P78 (collatz) where

import           Control.Monad.Writer
import           Data.Monoid (Sum(..))

{- | Starting from a positive integer \(n\),
we can have a sequence of numbers such that at each step,
the next number is \(3n+1\) if \(n\) is odd,
or \(\frac{n}{2}\) if \(n\) is even.
The Collatz conjecture states that this sequence will always end at 1
after a finite number of steps.

Using the t'Control.Monad.Writer.Lazy.Writer' monad, count the number of these steps
for a given positive integer \(n\).
-}
collatz :: Integral a => a -> a
collatz n = getSum $ execWriter $ collatz' n

collatz' :: Integral a => a -> Writer (Sum a) a
collatz' 1 = return 1
collatz' n = do
  tell 1
  if odd n
    then collatz' $ 3*n+1
    else collatz' $ n `div` 2
