"""
Day 12: Hill Climbing Algorithm
"""
# pylint: disable=invalid-name

from collections import deque


def _bfs(start, neighbors):
    seen = {start}
    queue = deque(((start, 0),))
    while queue:
        item, depth = queue.popleft()
        yield item, depth
        for neighbor in neighbors(item):
            if neighbor in seen:
                continue
            seen.add(neighbor)
            queue.append((neighbor, depth + 1))


def part1(lines):
    """
    >>> part1(["Sabqponm", "abcryxxl", "accszExk", "acctuvwj", "abdefghi"])
    31
    """
    (start,) = (
        (y, x) for y, line in enumerate(lines) for x, c in enumerate(line) if c == "S"
    )

    def neighbors(point):
        y0, x0 = point
        a = lines[y0][x0]
        for y1 in range(y0 - 1, y0 + 2):
            if y1 not in range(len(lines)):
                continue
            for x1 in range(x0 - 1, x0 + 2) if y0 == y1 else (x0,):
                if x1 not in range(len(lines[y1])):
                    continue
                b = lines[y1][x1]
                if (
                    b == "a"
                    if a == "S"
                    else a == "z"
                    if b == "E"
                    else ord(b) - ord(a) <= 1
                ):
                    yield y1, x1

    for (y, x), depth in _bfs(start, neighbors):
        if lines[y][x] == "E":
            return depth
    return None


def part2(lines):
    """
    >>> part2(["Sabqponm", "abcryxxl", "accszExk", "acctuvwj", "abdefghi"])
    29
    """
    (start,) = (
        (y, x) for y, line in enumerate(lines) for x, c in enumerate(line) if c == "E"
    )

    def neighbors(point):
        y0, x0 = point
        a = lines[y0][x0]
        for y1 in range(y0 - 1, y0 + 2):
            if y1 not in range(len(lines)):
                continue
            for x1 in range(x0 - 1, x0 + 2) if y0 == y1 else (x0,):
                if x1 not in range(len(lines[y1])):
                    continue
                b = lines[y1][x1]
                if b == "z" if a == "E" else ord(a) - ord(b) <= 1:
                    yield y1, x1

    for (y, x), depth in _bfs(start, neighbors):
        if lines[y][x] == "a":
            return depth
    return None


parts = (part1, part2)
