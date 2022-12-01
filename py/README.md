# [Advent of Code 2022](https://adventofcode.com/2022)
### my answers in [Python](https://www.python.org/) ![Python CI](https://github.com/ephemient/aoc2022/workflows/Python%20CI/badge.svg)

This project builds with [Poetry](https://python-poetry.org/).

Setup:

```sh
curl -sSL https://install.python-poetry.org | python3 -
poetry install
```

Run the test suite:

```sh
poetry run pytest
```

Run the benchmarks:

```sh
poetry run pytest --benchmark-enable
```

Print solutions for the inputs provided in local data files:

```sh
poetry run aoc2022
```

Lint and format code with [Black](https://black.readthedocs.io/), [pylint](https://github.com/PyCQA/pylint), and [isort](https://pycqa.github.io/isort/):

```sh
poetry run black .
poetry run pylint aoc2022
poetry run isort .
```
