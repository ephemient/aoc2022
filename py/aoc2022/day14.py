"""
Day 14: Regolith Reservoir
"""

SAMPLE_INPUT = [
    "498,4 -> 498,6 -> 496,6",
    "503,4 -> 502,4 -> 502,9 -> 494,9",
]


def _parse(lines):
    blocks = set()
    for line in lines:
        points = [tuple(map(int, point.split(","))) for point in line.split(" -> ")]
        for point1, point2 in zip(points, points[1:]):
            x1, y1 = point1
            x2, y2 = point2
            blocks.update(
                (x, y)
                for x in range(min(x1, x2), max(x1, x2) + 1)
                for y in range(min(y1, y2), max(y1, y2) + 1)
            )
    return blocks


def _fall(blocks, max_y):
    x = 500
    for y in range(0, max_y):
        if (x, y + 1) not in blocks:
            pass
        elif (x - 1, y + 1) not in blocks:
            x -= 1
        elif (x + 1, y + 1) not in blocks:
            x += 1
        else:
            return x, y
    return x, max_y


def part1(lines):
    """
    >>> part1(SAMPLE_INPUT)
    24
    """
    blocks = _parse(lines)
    max_y, count = max(y for _, y in blocks), 0
    while True:
        x, y = _fall(blocks, max_y)
        if y >= max_y:
            return count
        blocks.add((x, y))
        count += 1


def part2(lines):
    """
    >>> part2(SAMPLE_INPUT)
    93
    """
    blocks = _parse(lines)
    max_y, count = max(y for _, y in blocks) + 1, 0
    while True:
        x, y = _fall(blocks, max_y)
        blocks.add((x, y))
        count += 1
        if x == 500 and y == 0:
            return count


parts = (part1, part2)
