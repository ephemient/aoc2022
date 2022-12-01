"""
Advent of Code 2022 - my answers in Python
"""

import io
import sys

import pkg_resources


def main():
    # pylint: disable=missing-function-docstring
    args = set(int(arg) for arg in sys.argv[1:] if arg.isnumeric())
    days = pkg_resources.get_entry_map("aoc2022", "aoc2022.days")
    for day, entry in sorted(days.items(), key=lambda item: int(item[0])):
        if args and int(day) not in args:
            continue
        print(f"Day {day}")
        with io.TextIOWrapper(
            pkg_resources.resource_stream("aoc2022", f"day{day}.txt")
        ) as handle:
            data = handle.readlines()
        for part in entry.load():
            print(part(data))
        print()


if __name__ == "__main__":
    main()
