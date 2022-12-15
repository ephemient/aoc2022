"""
Day 15: Beacon Exclusion Zone
"""

import bisect
import operator
import re

SAMPLE_INPUT = [
    "Sensor at x=2, y=18: closest beacon is at x=-2, y=15",
    "Sensor at x=9, y=16: closest beacon is at x=10, y=16",
    "Sensor at x=13, y=2: closest beacon is at x=15, y=3",
    "Sensor at x=12, y=14: closest beacon is at x=10, y=16",
    "Sensor at x=10, y=20: closest beacon is at x=10, y=16",
    "Sensor at x=14, y=17: closest beacon is at x=10, y=16",
    "Sensor at x=8, y=7: closest beacon is at x=2, y=10",
    "Sensor at x=2, y=0: closest beacon is at x=2, y=10",
    "Sensor at x=0, y=11: closest beacon is at x=2, y=10",
    "Sensor at x=20, y=14: closest beacon is at x=25, y=17",
    "Sensor at x=17, y=20: closest beacon is at x=21, y=22",
    "Sensor at x=16, y=7: closest beacon is at x=15, y=3",
    "Sensor at x=14, y=3: closest beacon is at x=15, y=3",
    "Sensor at x=20, y=1: closest beacon is at x=15, y=3",
]


# pylint: disable=missing-class-docstring,missing-function-docstring
class Intervals:
    def __init__(self):
        self._data = []

    def __len__(self):
        return sum(len(range(start, stop)) for start, stop in self._data)

    def add(self, start, stop):
        i = bisect.bisect_left(self._data, start, key=operator.itemgetter(1))
        j = bisect.bisect_right(self._data, stop, key=operator.itemgetter(0))
        if i < j:
            start = min(start, self._data[i][0])
            stop = max(stop, self._data[j - 1][1])
        self._data[i:j] = [(start, stop)]

    def itergaps(self, start, stop):
        x = start
        for span in self._data:
            yield from range(x, span[0])
            x = span[1]
        yield from range(x, stop)


INTS = re.compile(r"-?\d+")


def part1(lines, y=2000000):
    """
    >>> part1(SAMPLE_INPUT, y=10)
    26
    """
    intervals, beacons = Intervals(), set()
    for line in lines:
        x0, y0, x1, y1 = (int(match.group(0)) for match in INTS.finditer(line))
        delta = abs(x0 - x1) + abs(y0 - y1) - abs(y - y0)
        if delta >= 0:
            intervals.add(x0 - delta, x0 + delta + 1)
        if y == y1:
            beacons.add(x1)
    return len(intervals) - len(beacons)


def part2(lines, size=4000000):
    """
    >>> part2(SAMPLE_INPUT, size=20)
    56000011
    """
    inputs = [[int(match.group(0)) for match in INTS.finditer(line)] for line in lines]
    for y in range(size + 1):
        intervals = Intervals()
        for x0, y0, x1, y1 in inputs:
            delta = abs(x0 - x1) + abs(y0 - y1) - abs(y - y0)
            start = max(0, x0 - delta)
            stop = min(size, x0 + delta) + 1
            if start < stop:
                intervals.add(start, stop)
        for x in intervals.itergaps(0, size + 1):
            return 4000000 * x + y
    return None


parts = (part1, part2)
