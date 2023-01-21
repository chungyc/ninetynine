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

-- $
-- The "Problems" module contains exactly 99 Haskell problems which you can try to solve.

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
-- > sumNumbers = Solution.sumNumbers
--
-- This is the function you would be implementing for the problem.
-- Initially, it will point to a function which already solves the problem.
-- You will be replacing this with your own solution.
-- Let's say that you decide to implement a recursive solution which
-- adds numbers as they are counted down:
--
-- > sumNumbers :: Integer -> Integer
-- > sumNumbers 1 = 1
-- > sumNumbers n = n + sumNumbers (n-1)
--
-- You can then run tests to verify whether the solution is correct.
-- This can be done by passing a @--match@ flag with the problem number
-- to @stack test@ inside a @--test-arguments@ flag.  For instance,
-- with the problem in the @Problems.P01@ module, you could pass in @--match=P01@.
-- The problem in this tutorial is in the @Problems.Tutorial@ module, so its tests
-- can be executed as follows:
--
-- > $ stack test --test-arguments="--match=Tutorial"
-- > ...
-- > ninetynine> test (suite: examples-test, args: --match=Tutorial)
-- >
-- > Examples: 3  Tried: 3  Errors: 0  Unexpected output: 0
-- >
-- > ninetynine> Test suite examples-test passed
-- > ninetynine> test (suite: ninetynine-test, args: --match=Tutorial)
-- >
-- >
-- > Problems.Tutorial
-- >   sumNumbers
-- >     is one when adding just one [✔]
-- >       +++ OK, passed 1 test.
-- >     adds a number to the sum [✔]
-- >       +++ OK, passed 100 tests.
-- >   Examples
-- >     sumNumbers 5 == 1 + 2 + 3 + 4 + 5 [✔]
-- >     sumNumbers 100 [✔]
-- > ...
-- > 10 examples, 0 failures
-- >
-- > ninetynine> Test suite ninetynine-test passed
-- > Completed 2 action(s).

-- ** Trying multiple solutions

-- *** Tests

-- *** Benchmarks

-- ** Advanced topics

-- $
-- The tests are implemented using [Hspec](https://hspec.github.io/),
-- with unit tests based on [HUnit](https://hackage.haskell.org/package/HUnit)
-- and property-based tests based on [QuickCheck](https://hackage.haskell.org/package/QuickCheck).
-- The benchmarks are implemented using [criterion](https://hackage.haskell.org/package/criterion).
