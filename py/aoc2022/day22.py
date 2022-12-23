"""
Day 22: Monkey Map
"""

import itertools
import math
import operator
import re

SAMPLE_INPUT = [
    "        ...#",
    "        .#..",
    "        #...",
    "        ....",
    "...#.......#",
    "........#...",
    "..#....#....",
    "..........#.",
    "        ...#....",
    "        .....#..",
    "        .#......",
    "        ......#.",
    "",
    "10R5L5R10L4R5L5",
]

PATTERN = re.compile(r"(\d+)|([LR])")


def _parse(lines):
    lines = iter(lines)
    board = [""]
    for line in lines:
        line = line.rstrip()
        if not line:
            break
        board.append(f" {line} ")
    board.append("")
    max_len = max(len(line) for line in board)
    for i, line in enumerate(board):
        board[i] = line.ljust(max_len, " ")
    moves = [
        int(match.group(1)) if match.group(1) else match.group(2)
        for match in re.finditer(PATTERN, next(lines))
    ]
    return board, moves


def _perimeter(board: list[str]):
    initial_x = board[1].index(".")
    x, y, direction = initial_x, 1, 0
    while True:
        yield x, y, direction
        forward_x = (x + 1, x, x - 1, x)[direction]
        forward_y = (y, y + 1, y, y - 1)[direction]
        if board[forward_y][forward_x] == " ":
            direction = (direction + 1) % 4
        else:
            left_x = (x + 1, x + 1, x - 1, x - 1)[direction]
            left_y = (y - 1, y + 1, y + 1, y - 1)[direction]
            if board[left_y][left_x] == " ":
                x, y = forward_x, forward_y
            else:
                x, y, direction = left_x, left_y, (direction - 1) % 4
        if x == initial_x and y == 1 and direction == 0:
            break


def _run(board, moves, warps):
    x, y, direction = board[1].index("."), 1, 0
    for move in moves:
        match move:
            case "L":
                direction = (direction - 1) % 4
            case "R":
                direction = (direction + 1) % 4
            case _:
                for _ in range(move):
                    next_x, next_y, next_direction = (
                        warps[x, y, direction]
                        if (x, y, direction) in warps
                        else (
                            (x + 1, x, x - 1, x)[direction],
                            (y, y + 1, y, y - 1)[direction],
                            direction,
                        )
                    )
                    if board[next_y][next_x] != ".":
                        break
                    x, y, direction = next_x, next_y, next_direction
    return 1000 * y + 4 * x + direction


def part1(lines):
    """
    >>> part1(SAMPLE_INPUT)
    6032
    """
    board, moves = _parse(lines)
    warps = {}
    for x, y, direction in _perimeter(board):
        warp_x, warp_y, direction = x, y, (direction - 1) % 4
        if direction == 0:
            warp_x = next(x for x in range(len(board[y])) if board[y][x] != " ")
        elif direction == 1:
            warp_y = next(y for y in range(len(board)) if board[y][x] != " ")
        elif direction == 2:
            warp_x = next(
                x for x in range(len(board[y]) - 1, -1, -1) if board[y][x] != " "
            )
        elif direction == 3:
            warp_y = next(
                y for y in range(len(board) - 1, -1, -1) if board[y][x] != " "
            )
        warps[x, y, direction] = warp_x, warp_y, direction
    return _run(board, moves, warps)


def part2(lines):
    # pylint: disable=too-many-locals
    """
    >>> part2(SAMPLE_INPUT)
    5031
    """
    board, moves = _parse(lines)
    edges = [
        (key, list(edge))
        for key, edge in itertools.groupby(_perimeter(board), operator.itemgetter(2))
    ]
    side_length = math.gcd(*(len(edge) for _, edge in edges))
    edges = [
        (direction, edge[i : i + side_length])
        for direction, edge in edges
        for i in range(0, len(edge), side_length)
    ]
    pairs = []
    while edges:
        i = 0
        while i < len(edges) - 1:
            direction1, edge1 = edges[i]
            direction2, edge2 = edges[i + 1]
            if (direction1 - direction2) % 4 == 1:
                pairs.append((edge1, edge2))
                edges[i : i + 2] = []
                edges[i:] = (
                    ((direction - 1) % 4, edge) for direction, edge in edges[i:]
                )
            else:
                i += 1
    warps = {}
    for edge1, edge2 in pairs:
        for point1, point2 in zip(edge1, edge2[::-1]):
            x1, y1, direction1 = point1
            x2, y2, direction2 = point2
            warps[x1, y1, (direction1 - 1) % 4] = x2, y2, (direction2 + 1) % 4
            warps[x2, y2, (direction2 - 1) % 4] = x1, y1, (direction1 + 1) % 4
    return _run(board, moves, warps)


parts = (part1, part2)

if __name__ == "__main__":
    import fileinput

    _lines = list(fileinput.input())
    print(part1(_lines))
    print(part2(_lines))
