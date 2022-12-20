"""
Day 20: Grove Positioning System
"""


def _mix(tuples: list[tuple[int, int]]):
    for i in range(len(tuples)):
        i, entry = next((j, entry) for j, entry in enumerate(tuples) if entry[0] == i)
        j = (i + entry[1]) % (len(tuples) - 1)
        if i < j:
            tuples[i:j] = tuples[i + 1 : j + 1]
        elif i > j:
            tuples[j + 1 : i + 1] = tuples[j:i]
        tuples[j] = entry


def part1(lines):
    """
    >>> part1(["1", "2", "-3", "3", "-2", "0", "4"])
    3
    """
    tuples = list(enumerate(map(int, lines)))
    _mix(tuples)
    i = next(i for i, entry in enumerate(tuples) if entry[1] == 0)
    return sum(tuples[(i + x) % len(tuples)][1] for x in range(1000, 3001, 1000))


def part2(lines):
    """
    >>> part2(["1", "2", "-3", "3", "-2", "0", "4"])
    1623178306
    """
    tuples = list(enumerate(811589153 * int(line) for line in lines))
    for _ in range(10):
        _mix(tuples)
    i = next(i for i, entry in enumerate(tuples) if entry[1] == 0)
    return sum(tuples[(i + x) % len(tuples)][1] for x in range(1000, 3001, 1000))


parts = (part1, part2)
