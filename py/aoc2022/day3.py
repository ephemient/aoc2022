"""
Day 3: Rucksack Reorganization
"""

SAMPLE_INPUT = [
    "vJrwpWtwJgWrhcsFMMfFFhFp",
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
    "PmmdzqPrVvPwwTWBwg",
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
    "ttgJtRGJQctTZtZT",
    "CrZsJsPPZsGzwwsLwLmpwMDw",
]


def _prio(item):
    if "a" <= item <= "z":
        return ord(item) - ord("a") + 1
    if "A" <= item <= "Z":
        return ord(item) - ord("A") + 27
    raise RuntimeError(f"not ok: {item}")


def part1(lines):
    """
    >>> part1(SAMPLE_INPUT)
    157
    """
    total = 0
    for line in lines:
        line = line.strip()
        for item in set(line[: len(line) // 2]).intersection(line[len(line) // 2 :]):
            total += _prio(item)
    return total


def part2(lines):
    """
    >>> part2(SAMPLE_INPUT)
    70
    """
    total = 0
    for first, second, third in zip(*(map(str.strip, lines),) * 3):
        total += _prio(set(first).intersection(second).intersection(third).pop())
    return total


parts = (part1, part2)
