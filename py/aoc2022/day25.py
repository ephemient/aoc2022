"""
Day 25: Full of Hot Air
"""

SAMPLE_INPUT = [
    "1=-0-2",
    "12111",
    "2=0=",
    "21",
    "2=01",
    "111",
    "20012",
    "112",
    "1=-1=",
    "1-12",
    "12",
    "1=",
    "122",
]


def _unsnafu(snafu):
    number = 0
    for char in snafu:
        number = 5 * number + (-2 if char == "=" else -1 if char == "-" else int(char))
    return number


def _snafu(number):
    snafu = ""
    while number:
        snafu += "012=-"[number % 5]
        number = (number + 2) // 5
    return snafu[::-1]


def part1(lines):
    """
    >>> part1(SAMPLE_INPUT)
    '2=-1=0'
    """
    return _snafu(sum(_unsnafu(line.rstrip()) for line in lines))


parts = (part1,)
