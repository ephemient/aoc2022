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
        # pylint: disable=too-many-return-statements
        if name == "humn":
            return Fraction(1), Fraction(0)
        match definitions[name]:
            case value if isinstance(value, Number):
                return Fraction(0), Fraction(value)
            case (lhs, "+", rhs):
                slope1, intercept1 = evaluate(lhs)
                slope2, intercept2 = evaluate(rhs)
                return slope1 + slope2, intercept1 + intercept2
            case (lhs, "-", rhs):
                slope1, intercept1 = evaluate(lhs)
                slope2, intercept2 = evaluate(rhs)
                return slope1 - slope2, intercept1 - intercept2
            case (lhs, "*", rhs):
                slope1, intercept1 = evaluate(lhs)
                slope2, intercept2 = evaluate(rhs)
                if not slope1:
                    return intercept1 * slope2, intercept1 * intercept2
                if not slope2:
                    return slope1 * intercept2, intercept1 * intercept2
            case (lhs, "/", rhs):
                slope1, intercept1 = evaluate(lhs)
                slope2, intercept2 = evaluate(rhs)
                if not slope2:
                    return slope1 / intercept2, intercept1 / intercept2

    lhs, _, rhs = definitions["root"]
    slope1, intercept1 = evaluate(lhs)
    slope2, intercept2 = evaluate(rhs)
    x = (intercept2 - intercept1) / (slope1 - slope2)
    assert x.denominator == 1
    return int(x)


parts = (part1, part2)
