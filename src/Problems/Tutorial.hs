{- |
Description: Tutorial for solving problems
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

A tutorial for implementing and testing solutions for 99 Haskell "Problems".
-}
module Problems.Tutorial where

import qualified Solutions.Tutorial as Solution

-- * Tutorial

{- $
The "Problems" module contains exactly 99 Haskell problems which you can try to solve.
One way to use the module is simply to use its documentation as a standalone list
of problems for which you can implement solutions from scratch.
If this is how you intend to solve the problems, then you can stop reading the tutorial here.

These modules were written in a way so that solutions can be tested and benchmarked.
The rest of the tutorial will guide you as to how this can be done.
-}

-- ** Solving a problem

{- $
Problems are given in the form of function documentation.
They will explain what is expected to be implemented for the problem.
They will also usually include examples for what the implemented functions should return.
For example, we can have a problem such as the following:
-}

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

{- $
If you look at the source, you will see something like this:

@
{- | Add the integers from 1 to a given number @n@.
...
-}
sumNumbers :: Integer -> Integer
sumNumbers = Solution.sumNumbers
@

This is the function you would be implementing for the problem.
Initially, it will point to a function which already solves the problem.
You will be replacing this with your own solution.
Let's say that we decide to implement a recursive solution which
adds numbers as they are counted down:

@
sumNumbers :: Integer -> Integer
sumNumbers 1 = 0
sumNumbers n = n + sumNumbers (n-1)
@

We can then run tests to verify whether the solution is correct.
This can be done by passing a @--match@ flag with the problem number
to @stack test@ inside a @--test-arguments@ flag.  For instance,
with the problem in the @Problems.P01@ module, we could pass in @--match=P01@.
The problem in this tutorial is in the @Problems.Tutorial@ module, so its tests
can be executed as follows:

@
$ stack test --test-arguments="--match=Tutorial"
...
ninetynine> test (suite: examples-test, args: --match=Tutorial)

Progress 1/2: ...: failure in expression `sumNumbers 5 == 1 + 2 + 3 + 4 + 5'
expected: True
 but got: False
          ^
...
ninetynine> test (suite: ninetynine-test, args: --match=Tutorial)

Problems.Tutorial
  sumNumbers
    is one when adding just one [✘]
...
@

Uh-oh, tests fail.  Turns out @sumNumbers 1@ should have been 1, not 0,
so we can fix it to:

@
sumNumbers :: Integer -> Integer
sumNumbers 1 = 1
sumNumbers n = n + sumNumbers (n-1)
@

Running the tests again, this time they pass:

@
$ stack test --test-arguments="--match=Tutorial"
...
Examples: 3  Tried: 3  Errors: 0  Unexpected output: 0

ninetynine> Test suite examples-test passed
...

Problems.Tutorial
  sumNumbers
    is one when adding just one [✔]
      +++ OK, passed 1 test.
    adds a number to the sum [✔]
      +++ OK, passed 100 tests.
...
10 examples, 0 failures

ninetynine> Test suite ninetynine-test passed
Completed 2 action(s).
@

We have implemented a solution to this problem.
Now we can move on to another one in another problem module such as "Problems.P01".
If you are only interested in implementing a single solution for each problem,
you can stop reading the tutorial here.
-}

-- ** Trying multiple solutions

-- *** Tests

-- *** Benchmarks

-- ** Advanced topics

{- $
The tests are implemented using [Hspec](https://hspec.github.io/),
with unit tests based on [HUnit](https://hackage.haskell.org/package/HUnit)
and property-based tests based on [QuickCheck](https://hackage.haskell.org/package/QuickCheck).
The benchmarks are implemented using [criterion](https://hackage.haskell.org/package/criterion).
Most of the examples in the documentation are tested
using [doctest-parallel](https://hackage.haskell.org/package/doctest-parallel).

The test modules for each problem have the same name as the problem modules,
except for ending with @Spec@.
They are structured as follows:

* Parameterized property tests.

* Example tests.

* Test specification.

* Functions that support the tests, if any.

The benchmark modules for each problem have the same name as the problem modules,
except for ending with @Bench@.
They are structured as follows:

* Overall benchmark group definition.

* Parameterized benchmark group.

* Functions that support the benchmarks, if any.

An @Examples@ module is responsible for testing the examples in the documentation.
-}

-- ** In conclusion

{- $
This is not an exam;
you do not have to use this set of problems in any particular way.
Feel free to solve problems in any order.
You can implement solutions from scratch using nothing but the most
basic functions that Haskell provides, or almost trivially by using
a sophisticated library to do most of the work.
If you do not want to solve any problems but would simply like to
see how solutions to a problem can be implemented, you can do that, too.
You may not even be interested in solving any problems, but instead
use the solutions already available to practice writing tests in
your favorite testing framework.

Whatever you do, I hope this will have proven to be of some benefit,
whether by providing enjoyment, educational value, or anything else.
-}
