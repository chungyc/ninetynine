# Ninety-Nine Haskell Problems

Tests for 99 Haskell Problems.

## Build, test, benchmark, document

```shell
$ export PROBLEM=P01
$ stack build --test --bench --haddock \
    --test-arguments=--match=$PROBLEM \
    --benchmark-arguments=$PROBLEM
```
