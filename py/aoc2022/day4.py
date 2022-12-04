"""
Day 4: Camp Cleanup
"""

import re

_PATTERN = re.compile(r"(\d+)-(\d+),(\d+)-(\d+)")


def part1(lines):
    """
    >>> part1(["2-4,6-8", "2-3,4-5", "5-7,7-9", "2-8,3-7", "6-6,4-6", "2-6,4-8"])
    2
    """
    count = 0
    for line in lines:
        start1, end1, start2, end2 = map(int, re.match(_PATTERN, line).groups())
        if start1 <= start2 and end1 >= end2 or start1 >= start2 and end1 <= end2:
            count += 1
    return count


def part2(lines):
    """
    >>> part2(["2-4,6-8", "2-3,4-5", "5-7,7-9", "2-8,3-7", "6-6,4-6", "2-6,4-8"])
    4
    """
    count = 0
    for line in lines:
        start1, end1, start2, end2 = map(int, re.match(_PATTERN, line).groups())
        if start1 <= end2 and end1 >= start2:
            count += 1
    return count


parts = (part1, part2)
