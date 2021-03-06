# Ninety-Nine Haskell Problems

This is an implementation of the [Ninety-Nine Haskell Problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems).
In a way, this is a buildable version of the list of problems with tests and benchmarks.

**This is a work in progress.  It does not include all problems yet.**

Each problem is provided a skeleton where you can implement your own solutions.
For example, the module `Problems.P11` provides a skeleton in which you can
provide your own implementation.  Once implemented, you can run the existing tests
to confirm it is indeed a correct solution, or at least one without obvious bugs.
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
without having to reference the original list.  Pre-generated documentation is available
at https://ninetynine.haskell.chungyc.org/.

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

If you would like to build all code, run tests, and regenerate documentation
automatically after any change is saved, then the following command will do so.

```shell
$ stack build --test --bench --haddock --no-run-benchmarks --file-watch --watch-all
```

## Notes

There are not actually 99 problems.

This project does not have a standard line length, in that it does not try to fit
descriptive pose within a fixed number of columns.
Instead, line breaks are wherever I felt like putting them.
However, there is a hard limit of 120 characters per line.

There are some differences between the problems in this project and those
at [Ninety-Nine Haskell Problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems).

*   Some of the wording was rephrased to match more closely their context in Haskell.
    For example, "Problems.P10" mentions encoding to a list of tuples, not a list of lists.
*   Some problems are intentionally omitted or replaced, because they are
    uninteresting in the context of Haskell or they cannot be provided as
    problem skeletons that fit the framework of this project.
    Such problems will be noted as such.

For pending tasks, see the [TODO](doc/TODO.md).
