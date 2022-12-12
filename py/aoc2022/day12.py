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


def both_parts(lines):
    """
    >>> both_parts(["Sabqponm", "abcryxxl", "accszExk", "acctuvwj", "abdefghi"])
    (31, 29)
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
        src = ord("a" if src == "S" else "z" if src == "E" else src)
        for row1 in range(row0 - 1, row0 + 2):
            if row1 not in range(len(lines)):
                continue
            for col1 in range(col0 - 1, col0 + 2) if row0 == row1 else (col0,):
                if col1 not in range(len(lines[row1])):
                    continue
                dst = lines[row1][col1]
                dst = ord("a" if dst == "S" else "z" if dst == "E" else dst)
                if src - dst <= 1:
                    yield row1, col1

    part1, part2 = None, None
    for (row, col), depth in _bfs(start, neighbors):
        if lines[row][col] == "S" and part1 is None:
            part1 = depth
        elif lines[row][col] == "a" and part2 is None:
            part2 = depth
        if part1 is not None and part2 is not None:
            break
    return part1, part2


parts = (both_parts,)
