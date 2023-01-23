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
sumNumbers :: Integer -> Integer
sumNumbers = Solution.sumNumbers
@

This is the function you would be implementing for the problem.
Initially, it will point to a function which already solves the problem.
You will be replacing this with your own solution.
Let's say that you decide to implement a recursive solution which
adds numbers as they are counted down:

@
sumNumbers :: Integer -> Integer
sumNumbers 1 = 0
sumNumbers n = n + sumNumbers (n-1)
@

You can then run tests to verify whether the solution is correct.
This can be done by passing a @--match@ flag with the problem number
to @stack test@ inside a @--test-arguments@ flag.  For instance,
with the problem in the @Problems.P01@ module, you could pass in @--match=P01@.
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
so you can fix it to:

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

You have now correctly implemented a solution to this problem.
Now you can move on to another one, such as "Problems.P01".
If you are only interested in implementing a single solution for each problem,
you can stop reading the tutorial here.
-}

-- ** Trying multiple solutions

{- $
There may be times when you would like to try more than one solution to a problem.
For example, you may have remembered the apocryphal story of Gauss figuring
out how to quickly calculate the sum of the numbers from 1 to 100,
thwarting his math teacher who wanted to take a break while
the students spent their time on a menial calculation.
Let's say you would like to compare this approach to the naive approach of adding numbers one by one.
So in addition to the @sumNumbers@ function above, you implement the @sumNumbers'@ function:

@
sumNumbers' :: Integer -> Integer
sumNumbers' n = n * (n+1) \`div\` 2
@

We can test this function as well without removing the @sumNumbers@ function.
We can also run benchmarks to compare how the two functions perform.
-}

-- *** Tests

{- $
The tests for this tutorial problem is in the @Problems.TutorialSpec@ module.
If you inspect the source, you will see this function:

@
spec :: Spec
spec = parallel $ do
  properties Problem.sumNumbers \"sumNumbers\"
  examples
  ...
@

You can update this function to also test @sumNumbers'@ by adding another line:

@
spec :: Spec
spec = parallel $ do
  properties Problem.sumNumbers \"sumNumbers\"
  __properties Problem.sumNumbers' \"sumNumbers'\"__
  examples
  ...
@
-}

-- *** Benchmarks

{- $
Benchmarks are provided for most problem modules.
For this tutorial problem, you can add a benchmark for @sumNumbers'@
by adding another line to the @Problems.TutorialBench@ module:

@
group = bgroup \"Tutorial\"
  [ subgroup \"sumNumbers\" Problem.sumNumbers
  __, subgroup \"sumNumbers'\" Problem.sumNumbers'__
  , bgroup \"Solutions\"
@

You can run the benchmarks for a problem
by including the problem number in the benchmark arguments.
For example, you would run the benchmarks for the tutorial problem by
including "@Tutorial@" in the benchmark arguments, after which you may
see output such as this:

@
$ stack bench --benchmark-arguments=\"Tutorial\"
...
benchmarking Tutorial\/sumNumbers\/1000000
time                 222.9 ms   (206.2 ms .. 237.9 ms)
...
benchmarking Tutorial\/sumNumbers'\/1000000
time                 56.75 ns   (56.43 ns .. 57.06 ns)
...
@

From this, you can see that @sumNumbers'@ is much faster than @sumNumbers@
when adding the numbers from 1 to 1,000,000.

For another example of running benchmarks, execute the following command to
run the benchmarks for "Problems.P11":

@
$ stack bench --benchmark-arguments=\"P11\"
@
-}

-- ** In conclusion

{- $
This is not an exam;
you do not have to use this set of problems in any particular way.
Feel free to solve problems in any order, or skip those you find uninteresting.
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
