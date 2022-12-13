"""
Day 13: Distress Signal
"""
# pylint: disable=duplicate-code

from itertools import zip_longest
from numbers import Number

_TOKEN_OPEN = object()
_TOKEN_CLOSE = object()


def _parse(line):
    index = 0
    while index < len(line):
        if line[index] == "[":
            yield _TOKEN_OPEN
        elif line[index] == "]":
            yield _TOKEN_CLOSE
        elif line[index].isdigit():
            start_index, index = index, index + 1
            while line[index].isdigit():
                index += 1
            yield int(line[start_index:index])
            continue
        index += 1


# pylint: disable=too-many-return-statements,too-many-branches
def _compare(lhs, rhs):
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
        if left is _TOKEN_CLOSE and right is _TOKEN_CLOSE:
            continue
        if left is _TOKEN_CLOSE:
            return -1
        if right is _TOKEN_CLOSE:
            return 1
        if isinstance(left, Number) and isinstance(right, Number):
            if left < right:
                return -1
            if left > right:
                return 1
        elif isinstance(left, Number):
            depth = 0
            while right is _TOKEN_OPEN:
                depth += 1
                right = next(rhs)
            if isinstance(right, Number):
                if left < right:
                    return -1
                if left > right:
                    return 1
            else:
                return 1
            for _ in range(depth):
                if next(rhs) is not _TOKEN_CLOSE:
                    return -1
        elif isinstance(right, Number):
            depth = 0
            while left is _TOKEN_OPEN:
                depth += 1
                left = next(lhs)
            if isinstance(left, Number):
                if right < left:
                    return 1
                if right > left:
                    return -1
            else:
                return -1
            for _ in range(depth):
                if next(lhs) is not _TOKEN_CLOSE:
                    return 1
        elif left != right:
            raise AssertionError()


def part1(lines):
    """
    >>> from .day13 import SAMPLE_INPUT
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
    >>> from .day13 import SAMPLE_INPUT
    >>> part2(SAMPLE_INPUT)
    140
    """
    divider1 = (_TOKEN_OPEN, _TOKEN_OPEN, 2, _TOKEN_CLOSE, _TOKEN_CLOSE)
    divider2 = (_TOKEN_OPEN, _TOKEN_OPEN, 6, _TOKEN_CLOSE, _TOKEN_CLOSE)
    count1, count2 = 1, 1
    for line in filter(str.strip, lines):
        if _compare(_parse(line), iter(divider1)) < 0:
            count1 += 1
        elif _compare(_parse(line), iter(divider2)) < 0:
            count2 += 1
    return count1 * (count1 + count2)


parts = (part1, part2)
