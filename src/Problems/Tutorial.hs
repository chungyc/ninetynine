{- |
Description: Tutorial for solving problems
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Tutorial for solving "Problems".
-}
module Problems.Tutorial where

import qualified Solutions.Tutorial as Solution

-- * Tutorial

-- ** Solving a problem

-- $
-- Problems are given in the form of function documentation.
-- They will explain what is expected to be implemented for the problem.
-- They will also usually include examples for what the implemented functions should return.
-- For example, we can have a problem such as the following:

{- | Add the integers from 1 to a given number @n@.

=== Examples

>>> sumNumbers 5 == 1 + 2 + 3 + 4 + 5
True

>>> sumNumbers 100
5050

>>> sumNumbers 1000000
500000500000
-}
sumNumbers :: Integer -> Integer
sumNumbers = Solution.sumNumbers

-- $
-- If you look at the source, you will see something like this:
--
-- > {- | Add the integers from 1 to a given number @n@.
-- > ...
-- > -}
-- > sumNumbers :: Integer -> Integer
-- > sumNumbers = Solutions.sumNumbers
--
-- This is the function you would be implementing for the problem.

-- $
-- There are a few solutions implemented in "Solutions.Tutorial".

-- ** Trying multiple solutions

-- *** Tests

-- *** Benchmarks

-- ** Advanced topics

-- $
-- The tests are implemented using [Hspec](https://hspec.github.io/),
-- with unit tests based on [HUnit](https://hackage.haskell.org/package/HUnit)
-- and property-based tests based on [QuickCheck](https://hackage.haskell.org/package/QuickCheck).
-- The benchmarks are implemented using [criterion](https://hackage.haskell.org/package/criterion).
