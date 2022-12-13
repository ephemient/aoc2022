"""
Day 13: Distress Signal
"""

import ast
from collections.abc import Iterable
from itertools import zip_longest

SAMPLE_INPUT = [
    "[1,1,3,1,1]",
    "[1,1,5,1,1]",
    "",
    "[[1],[2,3,4]]",
    "[[1],4]",
    "",
    "[9]",
    "[[8,7,6]]",
    "",
    "[[4,4],4,4]",
    "[[4,4],4,4,4]",
    "",
    "[7,7,7,7]",
    "[7,7,7]",
    "",
    "[]",
    "[3]",
    "",
    "[[[]]]",
    "[[]]",
    "",
    "[1,[2,[3,[4,[5,6,7]]]],8,9]",
    "[1,[2,[3,[4,[5,6,0]]]],8,9]",
]


def _parse(line):
    if any(char not in "\n,0123456789[]" for char in line):
        raise ValueError()
    return ast.literal_eval(line)


def _compare(lhs, rhs):
    if not isinstance(lhs, Iterable) and not isinstance(rhs, Iterable):
        return 0 if lhs == rhs else -1 if lhs < rhs else 1
    try:
        lhs = iter(lhs)
    except TypeError:
        lhs = iter((lhs,))
    try:
        rhs = iter(rhs)
    except TypeError:
        rhs = iter((rhs,))
    while True:
        try:
            left = next(lhs)
        except StopIteration:
            try:
                next(rhs)
                return -1
            except StopIteration:
                return 0
        try:
            right = next(rhs)
        except StopIteration:
            return 1
        comparison = _compare(left, right)
        if comparison:
            return comparison


def part1(lines):
    """
    >>> part1(SAMPLE_INPUT)
    13
    """
    total = 0
    for i, pair in enumerate(zip_longest(*(iter(lines),) * 3)):
        if _compare(_parse(pair[0]), _parse(pair[1])) <= 0:
            total += i + 1
    return total


def part2(lines):
    """
    >>> part2(SAMPLE_INPUT)
    140
    """
    divider1, divider2 = [[2]], [[6]]
    count1, count2 = 1, 1
    for line in filter(str.strip, lines):
        packet = _parse(line)
        if _compare(packet, divider1) < 0:
            count1 += 1
        elif _compare(packet, divider2) < 0:
            count2 += 1
    return count1 * (count1 + count2)


parts = (part1, part2)
