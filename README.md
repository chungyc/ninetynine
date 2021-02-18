# Ninety-Nine Haskell Problems

This is an implementation of the [Ninety-Nine Haskell Problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems).
In a way, this is a buildable version of the list of problems with tests and benchmarks.

Each problem is provided a skeleton where you can implement your own solutions.
For example, the module `Problems.P11` provides a skeleton in which you can
provide your own implementation.  Once implemented, you can run the existing
tests that it is indeed a correct solution, or at least one without obvious bugs.
You can also run existing benchmarks against your solution, e.g.,
if you are curious how your amazingly clever but complicated solution performs.

Modules under `Solutions` are those implemented by yours truly.
The problem skeletons alias to these, so that tests against the problems can pass
despite not having been implemented yet.  Tests and benchmarks are parametized,
so that it is possible to test and benchmark multiple solutions for a problem
in a single code base.

This project is available on GitHub at https://github.com/chungyc/ninetynine.

## Usage

This is a Haskell project set up with
the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/).

### Getting started

Clone the repository:

```shell
$ git clone https://github.com/chungyc/ninetynine.git
$ cd ninetynine
```

### Documentation

To generate the documention with [Haddock](https://www.haskell.org/haddock/):

```shell
$ stack haddock
```

The documentation for the `Problems` module can serve as a standalone list of the problems
without having to reference the original list.

### Tests

To run all tests:

```shell
$ stack test
```

If you want to run tests for only a single problem:

```shell
$ EXPORT PROBLEM=P01
$ stack test --test-arguments=--match=$PROBLEM
```

The tests include testing of examples in documentation
using [`doctest`](https://hackage.haskell.org/package/doctest).
If you do not want to include them, e.g., you are unable or unwilling
to install libraries necessary for building [GHCi](https://wiki.haskell.org/GHC/GHCi),
then they can be skipped by adding `--skip examples-test` to the arguments.

### Benchmarks

To run all benchmarks:

```shell
$ stack bench
```

By their nature, running all benchmarks takes significant time.
You can run benchmarks for a single problem as in the following,
which will also save the benchmark results in HTML.

```shell
$ export PROBLEM=P01
$ export BENCHMARKFILE=$HOME/benchmarks.html
$ stack bench --benchmark-arguments="--output=$BENCHMARKFILE $PROBLEM"
```

### Quick edit cycle

If you would like to build all code, run test, and regenerate documentation
automatically after any change is saved, then the following command will do so.

```shell
$ stack build --test --bench --haddock --no-run-benchmarks --file-watch --watch-all
```

## Notes

There are not actually 99 problems.

There are some differences between the problems in this project and those
at [Ninety-Nine Haskell Problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems).

*   Some of the wording was rephrased to match more closely their context in Haskell.
    For example, "Problems.P10" mentions encoding to a list of tuples, not a list of lists.
*   Some problems are intentionally omitted or replaced, because they are
    uninteresting in the context of Haskell or they cannot be provided as
    problem skeletons that fit the framework of this project.
    Such problems will be noted as such.

## Pending tasks

*   [ ] Provide implementations for all problems
    *   [x] Problem 1
    *   [x] Problem 2
    *   [x] Problem 3
    *   [x] Problem 4
    *   [x] Problem 5
    *   [x] Problem 6
    *   [x] Problem 7
    *   [x] Problem 8
    *   [x] Problem 9
    *   [x] Problem 10
    *   [x] Problem 11
    *   [x] Problem 12
    *   [x] Problem 13
    *   [x] Problem 14
    *   [x] Problem 15
    *   [x] Problem 16
    *   [ ] Problem 17
    *   [ ] Problem 18
    *   [ ] Problem 19
    *   [ ] Problem 20
    *   [ ] Problem 21
    *   [ ] Problem 22
    *   [ ] Problem 23
    *   [ ] Problem 24
    *   [ ] Problem 25
    *   [ ] Problem 26
    *   [ ] Problem 27
    *   [ ] Problem 28
    *   [x] Problem 31
    *   [x] Problem 32
    *   [ ] Problem 33
    *   [ ] Problem 34
    *   [ ] Problem 35
    *   [ ] Problem 36
    *   [ ] Problem 37
    *   [ ] Problem 38
    *   [ ] Problem 39
    *   [ ] Problem 40
    *   [ ] Problem 41
    *   [x] Problem 46
    *   [ ] Problem 48
    *   [ ] Problem 49
    *   [ ] Problem 50
    *   [ ] Problem 55
    *   [ ] Problem 56
    *   [ ] Problem 57
    *   [ ] Problem 58
    *   [ ] Problem 59
    *   [ ] Problem 60
    *   [ ] Problem 61
    *   [ ] Problem 62
    *   [ ] Problem 63
    *   [ ] Problem 64
    *   [ ] Problem 65
    *   [ ] Problem 66
    *   [ ] Problem 67
    *   [ ] Problem 68
    *   [ ] Problem 69
    *   [ ] Problem 70
    *   [ ] Problem 71
    *   [ ] Problem 72
    *   [ ] Problem 73
    *   [ ] Problem 80
    *   [ ] Problem 81
    *   [ ] Problem 82
    *   [ ] Problem 83
    *   [ ] Problem 84
    *   [ ] Problem 85
    *   [ ] Problem 86
    *   [ ] Problem 87
    *   [ ] Problem 88
    *   [ ] Problem 89
    *   [ ] Problem 90
    *   [ ] Problem 91
    *   [ ] Problem 92
    *   [ ] Problem 93
    *   [ ] Problem 94
    *   [ ] Problem 95
    *   [ ] Problem 96
    *   [ ] Problem 97
    *   [ ] Problem 98
    *   [ ] Problem 99
*   [ ] Replace problems
    *   [ ] Problem 47
    *   [ ] Problem 54
*   [ ] Add problems so that there are actually 99 problems
    *   [ ] Problem 29
    *   [ ] Problem 30
    *   [ ] Problem 42
    *   [ ] Problem 43
    *   [ ] Problem 44
    *   [ ] Problem 45
    *   [ ] Problem 51
    *   [ ] Problem 52
    *   [ ] Problem 53
    *   [ ] Problem 54
    *   [ ] Problem 74
    *   [ ] Problem 75
    *   [ ] Problem 76
    *   [ ] Problem 77
    *   [ ] Problem 78
    *   [ ] Problem 79
