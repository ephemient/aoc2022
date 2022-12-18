"""
Day 17: Pyroclastic Flow
"""

ROCKS = (
    (4, (15,)),
    (3, (2, 7, 2)),
    (3, (7, 4, 4)),
    (1, (1, 1, 1, 1)),
    (2, (3, 3)),
)


def _check_free(rock, lines, x, y):
    return all(not (line & row << x) for line, row in zip(lines[y:], rock))


def _step(width, rock, jet, jet_index, height, lines):
    # pylint: disable=too-many-arguments
    x, y = 2, len(lines) + 3
    while _check_free(rock, lines, x, y):
        match jet[jet_index]:
            case "<":
                x2 = x - 1
            case ">":
                x2 = x + 1
            case _:
                raise ValueError()
        jet_index = (jet_index + 1) % len(jet)
        if 0 <= x2 <= 7 - width and _check_free(rock, lines, x2, y):
            x = x2
        y -= 1
    y, lines = y + 1, list(lines)
    for rock_y, row in enumerate(rock):
        if y + rock_y >= len(lines):
            lines.append(row << x)
            height += 1
        else:
            lines[y + rock_y] = lines[y + rock_y] | row << x
    visible, queue = [0] * len(lines) + [1], [(0, len(lines))]
    while queue:
        x, y = queue.pop()
        for x2, y2 in ((x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1)):
            if (
                x2 in range(7)
                and y2 in range(len(visible))
                and not (visible[y2] & 1 << x2)
            ):
                visible[y2] |= 1 << x2
                if not (y2 < len(lines) and lines[y2] & 1 << x2):
                    queue.append((x2, y2))
    lines = [line & row for line, row in zip(lines, visible)]
    trim = 0
    while not lines[trim]:
        trim += 1
    return jet_index, height, lines[trim:]


def _solve(jet, count):
    jet_index, height, lines = 0, 0, [127]
    seen, heights = {}, []
    for i in range(count):
        rock_index = i % len(ROCKS)
        width, rock = ROCKS[rock_index]
        j = seen.setdefault((tuple(lines), jet_index, rock_index), i)
        if j < i:
            return heights[j + (count - j) % (i - j)] + (count - j) // (i - j) * (
                height - heights[j]
            )
        heights.append(height)
        jet_index, height, lines = _step(width, rock, jet, jet_index, height, lines)
    return height


def part1(lines):
    """
    >>> part1([">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"])
    3068
    """
    return _solve(next(iter(lines)).strip(), 2022)


def part2(lines):
    """
    >>> part2([">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"])
    1514285714288
    """
    return _solve(next(iter(lines)).strip(), 1000000000000)


parts = (part1, part2)
