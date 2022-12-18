# [Advent of Code 2022](https://adventofcode.com/2022)
### my answers in [Haskell](https://www.haskell.org/) ![Haskell CI](https://github.com/ephemient/aoc2022/workflows/Haskell%20CI/badge.svg)

This project builds with [The Haskell Cabal](https://www.haskell.org/cabal/).

Setup:

```sh
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
cabal configure --enable-tests
```

Run the [Hspec](https://hspec.github.io/) test suite:

```sh
cabal test aoc2022-test
```

Run [criterion](http://www.serpentine.com/criterion/) benchmarks ([results online](https://ephemient.github.io/aoc2022/aoc2022-bench.html)):

```sh
cabal bench aoc2022-bench
```

Print solutions for the inputs provided in local data files:

```sh
cabal run aoc2022-exe
```

Generate [Haddock](https://www.haskell.org/haddock/) API documentation:

```sh
stack haddock aoc2021:lib
```

Run [hlint](https://github.com/ndmitchell/hlint) source code suggestions:

```sh
stack build hlint --exec 'hlint src test bench'
```
