# Ninety-Nine Haskell Problems

This is an implementation of
the [Ninety-Nine Haskell Problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems).
In a way, this is a buildable version of
the [list of problems](https://ninetynine.haskell.chungyc.org/) with tests and benchmarks.

Each problem is provided a skeleton where you can implement your own solutions.
For example, the module `Problems.P11` provides a skeleton in which you can
provide your own implementation.  Once implemented, you can run the existing tests
to confirm it is indeed a correct solution, or at least one without obvious bugs.
You can also run existing benchmarks against your solution, e.g.,
if you are curious how your amazingly clever but complicated solution performs
compared to a simpler one.

Modules under `Solutions` are those implemented by yours truly.
The problem skeletons alias to these, so that tests against the problems can pass
despite not having been implemented yet.  Tests and benchmarks are parameterized,
so that it is possible to test and benchmark multiple solutions for a problem
in a single code base.

This project is available on [GitHub](https://github.com/chungyc/ninetynine).

## Usage

This is a Haskell project set up with
the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/).

### Getting started

Clone the repository:

```shell
$ git clone https://github.com/chungyc/ninetynine.git
$ cd ninetynine
```

Alternatively, you can try things out in a
[GitHub Codespace](https://docs.github.com/en/codespaces/overview).

[![Open in GitHub Codespaces](https://github.com/codespaces/badge.svg)](https://codespaces.new/chungyc/ninetynine?quickstart=1)

### Documentation

To generate the documention with [Haddock](https://www.haskell.org/haddock/):

```shell
$ stack haddock
```

The documentation for the `Problems` module can serve as a standalone list of the problems
without having to reference the original list.
[Pre-generated documentation](https://ninetynine.haskell.chungyc.org/) is also available.

### Tests

To run all tests:

```shell
$ stack test
```

If you want to run tests for a specific problem:

```shell
$ export PROBLEM=P01
$ stack test --test-arguments=--match=${PROBLEM}
```

The tests include testing of examples in documentation
using [`doctest`](https://hackage.haskell.org/package/doctest).
If you do not want to include them, e.g., you are unable or unwilling
to install libraries necessary for building [GHCi](https://wiki.haskell.org/GHC/GHCi),
then they can be skipped by adding `--skip examples-test` to the arguments.

### Benchmarks

To run benchmarks for a specific problem:

```shell
$ export PROBLEM=P01
$ stack bench --benchmark-arguments=${PROBLEM}
```

### Running interactively

To run code for a problem interactively:

```
$ stack ghci --no-load
...
Prelude> :load Problems.P01
...
*Problems.P01> myLast "abc"
'c'
*Problems.P01> ...
```

## License

This is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License,
or (at your option) any later version.

For more permissive use of the problems themselves, refer back to the original list of
Ninety-Nine Haskell Problems on
the [HaskellWiki](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems).

## Notes

This project does not have a standard line length, in that it does not try to fit
descriptive prose within a fixed number of columns.
Instead, line breaks are wherever I felt like putting them.
However, there is a hard limit of 120 characters per line if it is not problematic.

There are some differences between the problems in this project and those
at [Ninety-Nine Haskell Problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems).

*   Some of the wording was rephrased to match more closely their context in Haskell.
    For example, `Problems.P10` mentions encoding to a list of tuples, not a list of lists.

*   Some problems are intentionally omitted or replaced, because they are
    uninteresting in the context of Haskell or they cannot be provided as
    problem skeletons that fit the framework of this project.

    *   These are problems 38, 47, and 54.

*   A number of problems were added to fill out 99 problems.

    *   These are problems 29, 30, 42, 43, 44, 45, 51, 52, 53, 74, 75, 76, 77, 78, and 79.
