"""
Day 18: Boiling Boulders
"""

SAMPLE_INPUT = [
    "2,2,2",
    "1,2,2",
    "3,2,2",
    "2,1,2",
    "2,3,2",
    "2,2,1",
    "2,2,3",
    "2,2,4",
    "2,2,6",
    "1,2,5",
    "3,2,5",
    "2,1,5",
    "2,3,5",
]


def _parse(lines):
    for line in lines:
        x, y, z = line.rstrip().split(",", 2)
        yield int(x), int(y), int(z)


def _neighbors(point):
    x, y, z = point
    return [
        (x - 1, y, z),
        (x + 1, y, z),
        (x, y - 1, z),
        (x, y + 1, z),
        (x, y, z - 1),
        (x, y, z + 1),
    ]


def part1(lines):
    """
    >>> part1(SAMPLE_INPUT)
    64
    """
    points = set(_parse(lines))
    return sum(
        neighbor not in points for point in points for neighbor in _neighbors(point)
    )


def part2(lines):
    """
    >>> part2(SAMPLE_INPUT)
    58
    """
    points = set(_parse(lines))
    min_x, max_x = min(x for x, _, _ in points), max(x for x, _, _ in points)
    min_y, max_y = min(y for _, y, _ in points), max(y for _, y, _ in points)
    min_z, max_z = min(z for _, _, z in points), max(z for _, _, z in points)
    outside = {(min_x - 1, min_y - 1, min_z - 1)}
    queue = list(outside)
    while queue:
        for point in _neighbors(queue.pop()):
            x, y, z = point
            if (
                min_x - 1 <= x <= max_x + 1
                and min_y - 1 <= y <= max_y + 1
                and min_z - 1 <= z <= max_z + 1
                and point not in points
                and point not in outside
            ):
                outside.add(point)
                queue.append(point)
    return sum(
        neighbor in outside for point in points for neighbor in _neighbors(point)
    )


parts = (part1, part2)
