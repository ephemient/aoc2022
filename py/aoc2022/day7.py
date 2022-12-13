"""
Day 7: No Space Left On Device
"""

from collections import defaultdict

SAMPLE_INPUT = [
    "$ cd /",
    "$ ls",
    "dir a",
    "14848514 b.txt",
    "8504156 c.dat",
    "dir d",
    "$ cd a",
    "$ ls",
    "dir e",
    "29116 f",
    "2557 g",
    "62596 h.lst",
    "$ cd e",
    "$ ls",
    "584 i",
    "$ cd ..",
    "$ cd ..",
    "$ cd d",
    "$ ls",
    "4060174 j",
    "8033020 d.log",
    "5626152 d.ext",
    "7214296 k",
]


def _parse(lines):
    cwd, sizes = "", defaultdict(int)
    for line in lines:
        match line.split():
            case ("$", "cd", "/"):
                cwd = ""
            case ("$", "cd", ".."):
                cwd = cwd[: cwd.rindex("/") if "/" in cwd else 0]
            case ("$", "cd", path):
                cwd = f"{cwd}/{path}" if cwd else path
            case (size, *_):
                try:
                    size = int(size)
                except ValueError:
                    continue
                path = cwd
                while True:
                    sizes[path] += size
                    if not path:
                        break
                    path = path[: path.rindex("/") if "/" in path else 0]
    return sizes


def part1(lines):
    """
    >>> part1(SAMPLE_INPUT)
    95437
    """
    return sum(size for size in _parse(lines).values() if size <= 100000)


def part2(lines):
    """
    >>> part2(SAMPLE_INPUT)
    24933642
    """
    sizes = _parse(lines)
    total = sizes[""]
    return min(size for size in sizes.values() if 70000000 - (total - size) >= 30000000)


parts = (part1, part2)
