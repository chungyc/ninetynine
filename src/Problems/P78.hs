{- |
Description: Collatz conjecture
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P78".
-}
module Problems.P78 (collatz) where

import qualified Solutions.P78 as Solution

{- | Starting from a positive integer \(n\),
we can have a sequence of numbers such that at each step,
the next number is \(3n+1\) if \(n\) is odd,
or \(\frac{n}{2}\) if \(n\) is even.
The Collatz conjecture states that this sequence will always end at 1
after a finite number of steps.

Using the t'Control.Monad.Writer.Lazy.Writer' monad, count the number of these steps
for a given positive integer \(n\).

=== Examples

>>> collatz 1
0

>>> collatz 2
1

>>> collatz 31
106

=== __Hint__

The outputs produced by v'Control.Monad.Writer.Lazy.tell' with
the t'Control.Monad.Writer.Lazy.Writer' monad are combined with 'mappend'.
In the 'Data.Monoid.Sum' monoid, 'mappend' is addition.
-}
collatz :: Integral a => a -> a
collatz = Solution.collatz
