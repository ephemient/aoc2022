import io

import pkg_resources

from aoc2022 import day1, day2, day3, day4, day5, day6


def data(day):
    with io.TextIOWrapper(
        pkg_resources.resource_stream("aoc2022", f"day{day}.txt")
    ) as fh:
        return fh.readlines()


def test_day1_part1_bench(benchmark):
    benchmark(day1.part1, data(1))


def test_day1_part2_bench(benchmark):
    benchmark(day1.part2, data(1))


def test_day2_part1_bench(benchmark):
    benchmark(day2.part1, data(2))


def test_day2_part2_bench(benchmark):
    benchmark(day2.part2, data(2))


def test_day3_part1_bench(benchmark):
    benchmark(day3.part1, data(3))


def test_day3_part2_bench(benchmark):
    benchmark(day3.part2, data(3))


def test_day4_part1_bench(benchmark):
    benchmark(day4.part1, data(4))


def test_day4_part2_bench(benchmark):
    benchmark(day4.part2, data(4))


def test_day5_part1_bench(benchmark):
    benchmark(day5.part1, data(5))


def test_day5_part2_bench(benchmark):
    benchmark(day5.part2, data(5))


def test_day6_part1_bench(benchmark):
    benchmark(day6.part1, data(6))


def test_day6_part2_bench(benchmark):
    benchmark(day6.part2, data(6))
