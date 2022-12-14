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


def _fill(blocks, max_y):
    def _fill_helper(x, y):
        if (x, y) in blocks:
            return
        if y <= max_y:
            yield from _fill_helper(x, y + 1)
            yield from _fill_helper(x - 1, y + 1)
            yield from _fill_helper(x + 1, y + 1)
        blocks.add((x, y))
        yield x, y

    yield from _fill_helper(500, 0)


def both_parts(lines):
    """
    >>> both_parts(SAMPLE_INPUT)
    (24, 93)
    """
    blocks = _parse(lines)
    max_y = max(y for _, y in blocks)
    part1, part2 = None, 0
    for _, y in _fill(blocks, max_y):
        if y >= max_y and part1 is None:
            part1 = part2
        part2 += 1
    return part1, part2


parts = (both_parts,)
