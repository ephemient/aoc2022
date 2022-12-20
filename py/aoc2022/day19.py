"""
Day 19: Not Enough Minerals
"""

import itertools
import math
import re
import sys
from collections import defaultdict
from concurrent.futures import ProcessPoolExecutor

SAMPLE_INPUT = [
    "Blueprint 1: Each ore robot costs 4 ore. "
    "Each clay robot costs 2 ore. "
    "Each obsidian robot costs 3 ore and 14 clay. "
    "Each geode robot costs 2 ore and 7 obsidian.",
    "Blueprint 2: Each ore robot costs 2 ore. "
    "Each clay robot costs 3 ore. "
    "Each obsidian robot costs 3 ore and 8 clay. "
    "Each geode robot costs 3 ore and 12 obsidian.",
]

ID_PATTERN = re.compile(r"Blueprint (\d+):")
PART_PATTERN = re.compile(r"Each (\w+) robot ([^.]+)[.]")
COST_PATTERN = re.compile(r"(?:costs | and )(\d+) (\w+)")


def _parse(lines):
    for line in lines:
        yield int(re.match(ID_PATTERN, line).group(1)), {
            part_match.group(1): defaultdict(
                int,
                {
                    cost_match.group(2): int(cost_match.group(1))
                    for cost_match in re.finditer(COST_PATTERN, part_match.group(2))
                },
            )
            for part_match in re.finditer(PART_PATTERN, line)
        }


def _potential(blueprint, robots, materials, time):
    additional_robots = defaultdict(int)
    potential_materials = defaultdict(int, materials)
    for _ in range(time):
        for robot in blueprint:
            potential_materials[robot] += robots[robot] + additional_robots[robot]
        for robot, costs in blueprint.items():
            if all(
                potential_materials[material] >= cost * (additional_robots[robot] + 1)
                for material, cost in costs.items()
            ):
                additional_robots[robot] += 1
    return potential_materials["geode"]


def _geodes(blueprint, time):
    # pylint: disable=too-many-locals
    max_values = {}
    for costs in blueprint.values():
        for material, cost in costs.items():
            max_values[material] = max(max_values.get(material, 0), cost)
    best, queue = 0, [(defaultdict(int, {"ore": 1}), defaultdict(int), time)]
    while queue:
        robots, materials, time = queue.pop()
        if _potential(blueprint, robots, materials, time) < best:
            continue
        estimate = materials["geode"] + robots["geode"] * time
        if estimate > best:
            best = estimate
        for robot, costs in blueprint.items():
            if robot in max_values and robots[robot] >= max_values[robot]:
                continue
            delta = max(
                0
                if (demand := costs[material] - materials[material]) <= 0
                else math.inf
                if (supply := robots[material]) <= 0
                else (demand + supply - 1) // supply
                for material in blueprint
            )
            if delta < time:
                new_robots = defaultdict(int, robots)
                new_robots[robot] += 1
                new_materials = defaultdict(int, materials)
                for material, count in robots.items():
                    new_materials[material] += count * (delta + 1)
                for material, cost in costs.items():
                    new_materials[material] -= cost
                queue.append((new_robots, new_materials, time - delta - 1))
    return best


def _part1_worker(blueprint):
    bid, blueprint = blueprint
    geodes = _geodes(blueprint, 24)
    print((bid, geodes), file=sys.stderr)
    return bid * geodes


def part1(lines):
    """
    >>> part1(SAMPLE_INPUT)
    33
    """
    with ProcessPoolExecutor() as executor:
        return sum(executor.map(_part1_worker, _parse(lines)))


def _part2_worker(blueprint):
    bid, blueprint = blueprint
    geodes = _geodes(blueprint, 32)
    print((bid, geodes), file=sys.stderr)
    return geodes


def part2(lines):
    """
    >>> part2(SAMPLE_INPUT)
    3472
    """

    with ProcessPoolExecutor() as executor:
        return math.prod(
            executor.map(_part2_worker, itertools.islice(_parse(lines), 3))
        )


parts = (part1, part2)
