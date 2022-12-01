import io

import pkg_resources
from aoc2022 import day1


def data(day):
    with io.TextIOWrapper(
        pkg_resources.resource_stream("aoc2022", f"day{day}.txt")
    ) as fh:
        return fh.readlines()


def test_day1_part1_bench(benchmark):
    benchmark(day1.part1, data(1))


def test_day1_part2_bench(benchmark):
    benchmark(day1.part2, data(1))
