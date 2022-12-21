"""
Day 21: Monkey Math
"""

from fractions import Fraction
from numbers import Number

SAMPLE_INPUT = [
    "root: pppw + sjmn",
    "dbpl: 5",
    "cczh: sllz + lgvd",
    "zczc: 2",
    "ptdq: humn - dvpt",
    "dvpt: 3",
    "lfqf: 4",
    "humn: 5",
    "ljgn: 2",
    "sjmn: drzm * dbpl",
    "sllz: 4",
    "pppw: cczh / lfqf",
    "lgvd: ljgn * ptdq",
    "drzm: hmdt - zczc",
    "hmdt: 32",
]


def _parse(lines):
    definitions = {}
    for line in lines:
        name, definition = line.split(": ", maxsplit=1)
        try:
            definitions[name] = int(definition)
        except ValueError:
            definitions[name] = tuple(definition.rstrip().split(maxsplit=2))
    return definitions


def part1(lines):
    """
    >>> part1(SAMPLE_INPUT)
    152
    """
    definitions = _parse(lines)

    def evaluate(name):
        match definitions[name]:
            case value if isinstance(value, Number):
                return value
            case (lhs, "+", rhs):
                return evaluate(lhs) + evaluate(rhs)
            case (lhs, "-", rhs):
                return evaluate(lhs) - evaluate(rhs)
            case (lhs, "*", rhs):
                return evaluate(lhs) * evaluate(rhs)
            case (lhs, "/", rhs):
                return evaluate(lhs) // evaluate(rhs)

    return evaluate("root")


def part2(lines):
    """
    >>> part2(SAMPLE_INPUT)
    301
    """
    definitions = _parse(lines)

    def evaluate(name):
        if name == "humn":
            return Fraction(1), Fraction(0)
        match definitions[name]:
            case value if isinstance(value, Number):
                return Fraction(0), Fraction(value)
            case (lhs, "+", rhs):
                a, b = evaluate(lhs)
                c, d = evaluate(rhs)
                return a + c, b + d
            case (lhs, "-", rhs):
                a, b = evaluate(lhs)
                c, d = evaluate(rhs)
                return a - c, b - d
            case (lhs, "*", rhs):
                a, b = evaluate(lhs)
                c, d = evaluate(rhs)
                if not a:
                    return b * c, b * d
                if not c:
                    return a * d, b * d
            case (lhs, "/", rhs):
                a, b = evaluate(lhs)
                c, d = evaluate(rhs)
                if not c:
                    return a / d, b / d

    lhs, _, rhs = definitions["root"]
    m, b = evaluate(lhs)
    n, c = evaluate(rhs)
    x = (c - b) / (m - n)
    assert x.denominator == 1
    return int(x)


parts = (part1, part2)
