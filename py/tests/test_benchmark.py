"""
Benchmarks
"""

from importlib import metadata, resources


def _make_test_bench(day, part):
    def _test_bench(benchmark):
        with resources.files("aoc2022").joinpath(f"day{day}.txt").open() as file:
            data = file.readlines()
        benchmark(part, data)

    return _test_bench


def _make_test_benches():
    for entry in metadata.entry_points().select(group="aoc2022.days"):
        day = "".join(c for c in entry.name if c.isdigit())
        for part in entry.load():
            yield f"test_{entry.name}_{part.__name__}_bench", _make_test_bench(
                day, part
            )


globals().update(_make_test_benches())
