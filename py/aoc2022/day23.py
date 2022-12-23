"""
Day 23: Unstable Diffusion
"""

from collections import defaultdict
from itertools import cycle, islice

SAMPLE_INPUT = [
    "....#..",
    "..###.#",
    "#...#.#",
    ".#...##",
    "#.###..",
    "##.#.##",
    ".#..#..",
]


def _directions():
    return cycle(("NSWE", "SWEN", "WENS", "ENSW"))


def _sides(direction, x, y):
    if direction == "N":
        return [(x, y - 1) for x in range(x - 1, x + 2)]
    if direction == "S":
        return [(x, y + 1) for x in range(x - 1, x + 2)]
    if direction == "W":
        return [(x - 1, y) for y in range(y - 1, y + 2)]
    if direction == "E":
        return [(x + 1, y) for y in range(y - 1, y + 2)]
    raise ValueError()


def _move(direction, x, y):
    if direction == "N":
        return x, y - 1
    if direction == "S":
        return x, y + 1
    if direction == "W":
        return x - 1, y
    if direction == "E":
        return x + 1, y
    raise ValueError()


def _neighbors(x, y):
    return [
        (x2, y2)
        for x2 in range(x - 1, x + 2)
        for y2 in range(y - 1, y + 2)
        if x2 != x or y2 != y
    ]


def _parse(lines):
    return {
        (x, y) for y, line in enumerate(lines) for x, c in enumerate(line) if c == "#"
    }


def _step(state, directions):
    proposals = defaultdict(list)
    for x, y in state:
        if not any(neighbor in state for neighbor in _neighbors(x, y)):
            continue
        for direction in directions:
            if not any(neighbor in state for neighbor in _sides(direction, x, y)):
                proposals[_move(direction, x, y)].append((x, y))
                break
    state = set(state)
    for new, old in proposals.items():
        if len(old) != 1:
            continue
        state.remove(old[0])
        state.add(new)
    return state


def part1(lines):
    """
    >>> part1(SAMPLE_INPUT)
    110
    """
    state = _parse(lines)
    for directions in islice(_directions(), 10):
        state = _step(state, directions)
    min_x, max_x = min(x for x, _ in state), max(x for x, _ in state)
    min_y, max_y = min(y for _, y in state), max(y for _, y in state)
    return (max_y - min_y + 1) * (max_x - min_x + 1) - len(state)


def part2(lines):
    """
    >>> part2(SAMPLE_INPUT)
    20
    """
    state, previous = _parse(lines), None
    for i, directions in enumerate(_directions()):
        state = _step(state, directions)
        if state == previous:
            return i + 1
        previous = state
    raise LookupError()


parts = (part1, part2)
