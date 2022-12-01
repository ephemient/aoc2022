"""
Day 1: Calorie Counting
"""

SAMPLE_INPUT = [
    "1000",
    "2000",
    "3000",
    "",
    "4000",
    "",
    "5000",
    "6000",
    "",
    "7000",
    "8000",
    "9000",
    "",
    "10000",
]


def _parse(lines):
    sums = []
    acc = 0
    for line in lines:
        line = line.strip()
        if line:
            acc += int(line)
        else:
            sums.append(acc)
            acc = 0
    sums.append(acc)
    return sums


def part1(lines):
    """
    >>> part1(SAMPLE_INPUT)
    24000
    """
    return max(_parse(lines))


def part2(lines):
    """
    >>> part2(SAMPLE_INPUT)
    45000
    """
    return sum(sorted(_parse(lines))[-3:])


parts = (part1, part2)
