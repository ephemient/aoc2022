"""
Day 2: Rock Paper Scissors
"""


def _parse(lines):
    return (
        (ord(line[0]) - ord("A") + 1, ord(line[2]) - ord("X") + 1) for line in lines
    )


def _score(left, right):
    return (1 + right - left) % 3 * 3 + right


def part1(lines):
    """
    >>> part1(["A Y", "B X", "C Z"])
    15
    """
    return sum(_score(left, right) for left, right in _parse(lines))


def part2(lines):
    """
    >>> part2(["A Y", "B X", "C Z"])
    12
    """
    return sum(_score(left, 1 + (left + right) % 3) for left, right in _parse(lines))


parts = (part1, part2)
