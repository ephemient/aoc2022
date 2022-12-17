"""
Day 17: Pyroclastic Flow
"""

ROCKS = [
    ["####"],
    [".#.", "###", ".#."],
    ["###", "..#", "..#"],
    ["#", "#", "#", "#"],
    ["##", "##"],
]


def _check_free(rock, lines, x, y):
    for rock_y, row in enumerate(rock):
        if y + rock_y >= len(lines):
            return True
        line = lines[y + rock_y]
        for rock_x, char in enumerate(row):
            if char == "#" and line[x + rock_x] == "#":
                return False
    return True


def _step(rock, jet, jet_index, height, lines):
    # pylint: disable=too-many-branches,too-many-locals
    max_x = 7 - max(len(row) for row in rock)
    x, y = 2, len(lines) + 3
    while True:
        match jet[jet_index]:
            case "<":
                x2 = x - 1
            case ">":
                x2 = x + 1
            case _:
                raise ValueError()
        jet_index = (jet_index + 1) % len(jet)
        if 0 <= x2 <= max_x and _check_free(rock, lines, x2, y):
            x = x2
        if not _check_free(rock, lines, x, y - 1):
            break
        y -= 1
    lines = list(lines)
    for rock_y, row in enumerate(rock):
        if y + rock_y >= len(lines):
            lines.append(row.rjust(x + len(row), ".").ljust(7, "."))
            height += 1
        else:
            line = list(lines[y + rock_y])
            for rock_x, char in enumerate(row):
                if char == "#":
                    line[x + rock_x] = "#"
            lines[y + rock_y] = "".join(line)
    visible, queue = {(0, len(lines))}, [(0, len(lines))]
    while queue:
        x, y = queue.pop()
        if y < len(lines) and lines[y][x] == "#":
            continue
        if x > 0 and (x - 1, y) not in visible:
            visible.add((x - 1, y))
            queue.append((x - 1, y))
        if x < 6 and (x + 1, y) not in visible:
            visible.add((x + 1, y))
            queue.append((x + 1, y))
        if y > 0 and (x, y - 1) not in visible:
            visible.add((x, y - 1))
            queue.append((x, y - 1))
        if y < len(lines) and (x, y + 1) not in visible:
            visible.add((x, y + 1))
            queue.append((x, y + 1))
    lines = [
        "".join(c if (x, y) in visible else "." for x, c in enumerate(line))
        for y, line in enumerate(lines)
    ]
    trim = 0
    for trim, line in enumerate(lines):
        if "#" in line:
            break
    return jet_index, height, lines[trim:]


def _solve(jet, count):
    jet_index, height, lines = 0, 0, ["#######"]
    seen, heights = {}, []
    for i in range(count):
        j = seen.setdefault((tuple(lines), jet_index, i % len(ROCKS)), i)
        if j < i:
            return heights[j + (count - j) % (i - j)] + (count - j) // (i - j) * (
                height - heights[j]
            )
        heights.append(height)
        jet_index, height, lines = _step(
            ROCKS[i % len(ROCKS)], jet, jet_index, height, lines
        )
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
