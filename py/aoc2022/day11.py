"""
Day 11: Monkey in the Middle
"""

import functools
import operator
from collections.abc import Callable
from dataclasses import dataclass
from math import lcm

SAMPLE_INPUT = [
    "Monkey 0:",
    "  Starting items: 79, 98",
    "  Operation: new = old * 19",
    "  Test: divisible by 23",
    "  If true: throw to monkey 2",
    "  If false: throw to monkey 3",
    "",
    "Monkey 1:",
    "  Starting items: 54, 65, 75, 74",
    "  Operation: new = old + 6",
    "  Test: divisible by 19",
    "  If true: throw to monkey 2",
    "  If false: throw to monkey 0",
    "",
    "Monkey 2:",
    "  Starting items: 79, 60, 97",
    "  Operation: new = old * old",
    "  Test: divisible by 13",
    "  If true: throw to monkey 1",
    "  If false: throw to monkey 3",
    "",
    "Monkey 3:",
    "  Starting items: 74",
    "  Operation: new = old + 3",
    "  Test: divisible by 17",
    "  If true: throw to monkey 0",
    "  If false: throw to monkey 1",
]


@dataclass
class _Monkey:
    items: [int]
    operation: Callable[[int], int]
    test: int
    if_true: int
    if_false: int


def _square(x):
    return x * x


def _parse(lines):
    lines = iter(lines)
    try:
        while True:
            if next(lines).strip():
                items = [
                    int(word.rstrip(","))
                    for word in next(lines).split()
                    if word[0].isdigit()
                ]
                match next(lines).split()[-2:]:
                    case ["*", "old"]:
                        operation = _square
                    case ["*", mul]:
                        operation = functools.partial(operator.mul, int(mul))
                    case ["+", add]:
                        operation = functools.partial(operator.add, int(add))
                    case _:
                        raise ValueError()
                test = int(next(lines).split()[-1])
                if_true = int(next(lines).split()[-1])
                if_false = int(next(lines).split()[-1])
                yield _Monkey(items, operation, test, if_true, if_false)
    except StopIteration as _:
        pass


def _solve(monkeys, iterations, post):
    counts = [0] * len(monkeys)
    for j in range(iterations):
        for i, monkey in enumerate(monkeys):
            items = monkey.items[:]
            monkey.items[:] = []
            for item in items:
                item = post(monkey.operation(item))
                monkeys[
                    monkey.if_false if item % monkey.test else monkey.if_true
                ].items.append(item)
            counts[i] += len(items)
    counts.sort()
    return counts[-1] * counts[-2]


def part1(lines):
    """
    >>> part1(SAMPLE_INPUT)
    10605
    """
    return _solve(list(_parse(lines)), 20, lambda x: x // 3)


def part2(lines):
    """
    >>> part2(SAMPLE_INPUT)
    2713310158
    """
    monkeys = list(_parse(lines))
    base = lcm(*(monkey.test for monkey in monkeys))
    return _solve(monkeys, 10000, lambda x: x % base)


parts = (part1, part2)
