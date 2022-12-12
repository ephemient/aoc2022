"""
Day 12: Hill Climbing Algorithm
"""

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
        (row, col)
        for row, line in enumerate(lines)
        for col, c in enumerate(line)
        if c == "S"
    )

    def neighbors(point):
        row0, col0 = point
        src = lines[row0][col0]
        for row1 in range(row0 - 1, row0 + 2):
            if row1 not in range(len(lines)):
                continue
            for col1 in range(col0 - 1, col0 + 2) if row0 == row1 else (col0,):
                if col1 not in range(len(lines[row1])):
                    continue
                dst = lines[row1][col1]
                if (
                    dst == "a"
                    if src == "S"
                    else src == "z"
                    if dst == "E"
                    else ord(dst) - ord(src) <= 1
                ):
                    yield row1, col1

    for (row, col), depth in _bfs(start, neighbors):
        if lines[row][col] == "E":
            return depth
    return None


def part2(lines):
    """
    >>> part2(["Sabqponm", "abcryxxl", "accszExk", "acctuvwj", "abdefghi"])
    29
    """
    (start,) = (
        (row, col)
        for row, line in enumerate(lines)
        for col, c in enumerate(line)
        if c == "E"
    )

    def neighbors(point):
        row0, col0 = point
        src = lines[row0][col0]
        for row1 in range(row0 - 1, row0 + 2):
            if row1 not in range(len(lines)):
                continue
            for col1 in range(col0 - 1, col0 + 2) if row0 == row1 else (col0,):
                if col1 not in range(len(lines[row1])):
                    continue
                dst = lines[row1][col1]
                if dst == "z" if src == "E" else ord(src) - ord(dst) <= 1:
                    yield row1, col1

    for (row, col), depth in _bfs(start, neighbors):
        if lines[row][col] == "a":
            return depth
    return None


parts = (part1, part2)
