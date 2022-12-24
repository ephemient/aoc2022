"""
Day 24: Blizzard Basin
"""

import heapq

SAMPLE_INPUT = ["#.######", "#>>.<^<#", "#.<..<<#", "#>v.><>#", "#<^v^^>#", "######.#"]


def _search(lines, start, end, time=0):
    end_x, end_y = end
    seen, queue = {(start, time)}, [(0, (start, time))]
    while queue:
        position, time = heapq.heappop(queue)[1]
        if position == end:
            return time
        x, y = position
        time += 1
        for x, y in [(x - 1, y), (x, y - 1), (x, y), (x, y + 1), (x + 1, y)]:
            if y < 0 or y >= len(lines) or x < 1 or x >= len(line := lines[y]) - 1:
                continue
            if y in (0, len(lines) - 1):
                if line[x] != ".":
                    continue
            elif lines[y][(x - 1 + time) % (len(line) - 2) + 1] == "<":
                continue
            elif lines[y][(x - 1 - time) % (len(line) - 2) + 1] == ">":
                continue
            elif lines[(y - 1 + time) % (len(lines) - 2) + 1][x] == "^":
                continue
            elif lines[(y - 1 - time) % (len(lines) - 2) + 1][x] == "v":
                continue
            state = ((x, y), time)
            if state not in seen:
                seen.add(state)
                heapq.heappush(queue, (time + abs(x - end_x) + abs(y - end_y), state))
    raise LookupError()


def part1(lines):
    """
    >>> part1(SAMPLE_INPUT)
    18
    """
    lines = [line.rstrip() for line in lines]
    return _search(lines, (1, 0), (len(lines[-1]) - 2, len(lines) - 1))


def part2(lines):
    """
    >>> part2(SAMPLE_INPUT)
    54
    """
    lines = [line.rstrip() for line in lines]
    start = (1, 0)
    end = (len(lines[-1]) - 2, len(lines) - 1)
    return _search(
        lines, start, end, _search(lines, end, start, _search(lines, start, end))
    )


parts = (part1, part2)
