"""
Day 5: Supply Stacks
"""
from itertools import zip_longest

SAMPLE_INPUT = [
    "    [D]    ",
    "[N] [C]    ",
    "[Z] [M] [P]",
    " 1   2   3 ",
    "",
    "move 1 from 2 to 1",
    "move 3 from 1 to 3",
    "move 2 from 2 to 1",
    "move 1 from 1 to 2",
]


def _solve(reverse, lines):
    lines = [line.rstrip() for line in lines]
    middle = lines.index("")
    stacks = {
        stack[-1]: "".join(filter(None, stack[:-1])).lstrip()
        for stack in zip_longest(*lines[:middle])
        if stack[-1] and stack[-1].isdigit()
    }
    for line in lines[middle + 1 :]:
        _, num, _, source, _, dest = line.split()
        num = int(num)
        temp = stacks[source]
        stacks[source] = temp[num:]
        stacks[dest] = (temp[num - 1 :: -1] if reverse else temp[:num]) + stacks[dest]
    return "".join(stacks[key][0] for key in sorted(stacks))


def part1(lines):
    """
    >>> part1(SAMPLE_INPUT)
    'CMZ'
    """
    return _solve(True, lines)


def part2(lines):
    """
    >>> part2(SAMPLE_INPUT)
    'MCD'
    """
    return _solve(False, lines)


parts = (part1, part2)
