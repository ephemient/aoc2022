"""
Advent of Code 2022 - my answers in Python
"""

import sys
from importlib import metadata, resources

from natsort import natsorted


def main():
    # pylint: disable=missing-function-docstring
    names = set(f"day{arg}" for arg in sys.argv[1:] if arg.isnumeric())
    days = metadata.entry_points().select(group="aoc2022.days")
    for entry in natsorted(days, key=lambda entry: entry.name):
        day = entry.name
        if names and day not in names:
            continue
        print(f"Day {day.removeprefix('day')}")
        with resources.files("aoc2022").joinpath(f"{day}.txt").open() as file:
            data = file.readlines()
        for part in entry.load():
            print(part(data))
        print()


if __name__ == "__main__":
    main()
