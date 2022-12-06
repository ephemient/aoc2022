"""
Day 6: Tuning Trouble
"""


def _solve(size, data):
    for i in range(size, len(data) + 1):
        if len(set(data[i - size : i])) == size:
            return i
    return None


def part1(lines):
    """
    >>> part1(["mjqjpqmgbljsphdztnvjfqwrcgsmlb"])
    7
    >>> part1(["bvwbjplbgvbhsrlpgdmjqwftvncz"])
    5
    >>> part1(["nppdvjthqldpwncqszvftbrmjlhg"])
    6
    >>> part1(["nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"])
    10
    >>> part1(["zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"])
    11
    """
    return _solve(4, lines[0])


def part2(lines):
    """
    >>> part2(["mjqjpqmgbljsphdztnvjfqwrcgsmlb"])
    19
    >>> part2(["bvwbjplbgvbhsrlpgdmjqwftvncz"])
    23
    >>> part2(["nppdvjthqldpwncqszvftbrmjlhg"])
    23
    >>> part2(["nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"])
    29
    >>> part2(["zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"])
    26
    """
    return _solve(14, lines[0])


parts = (part1, part2)
