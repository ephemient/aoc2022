"""
Day 9: Rope Bridge
"""


def _moves(lines):
    current_x, current_y = 0, 0
    yield current_x, current_y
    for line in lines:
        direction, count = line.split()
        for _ in range(int(count)):
            match direction:
                case "L":
                    current_x -= 1
                case "R":
                    current_x += 1
                case "U":
                    current_y -= 1
                case "D":
                    current_y += 1
            yield current_x, current_y


def _follow(points):
    tail_x, tail_y = 0, 0
    yield tail_x, tail_y
    for head_x, head_y in points:
        delta_x, delta_y = head_x - tail_x, head_y - tail_y
        if abs(delta_x) <= 1 and abs(delta_y) <= 1:
            continue
        if abs(delta_x) < abs(delta_y):
            tail_x, tail_y = head_x, head_y - delta_y // abs(delta_y)
        elif abs(delta_x) > abs(delta_y):
            tail_x, tail_y = head_x - delta_x // abs(delta_x), head_y
        else:
            tail_x, tail_y = head_x - delta_x // abs(delta_x), head_y - delta_y // abs(
                delta_y
            )
        yield tail_x, tail_y


def part1(lines):
    """
    >>> part1(["R 4", "U 4", "L 3", "D 1", "R 4", "D 1", "L 5", "R 2"])
    13
    """
    return len(set(_follow(_moves(lines))))


def part2(lines):
    """
    >>> part2(["R 5", "U 8", "L 8", "D 3", "R 17", "D 10", "L 25", "U 20"])
    36
    """
    # fmt: off
    # pylint: disable=line-too-long
    return len(set(_follow(_follow(_follow(_follow(_follow(_follow(_follow(_follow(_follow(_moves(lines))))))))))))
    # fmt: on


parts = (part1, part2)
