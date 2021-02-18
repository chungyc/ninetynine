# Ninety-Nine Haskell Problems

These are solutions, tests, and benchmarks
for [Ninety-Nine Haskell Problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems).

This project is available on GitHub at https://github.com/chungyc/ninetynine.

## Usage

This is a Haskell project set up with
the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/).

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
