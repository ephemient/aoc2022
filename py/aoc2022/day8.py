"""
Day 8: Treetop Tree House
"""


def _scan_visibility(line):
    max_height = None
    for height in line:
        if max_height is None or height > max_height:
            max_height = height
            yield True
        else:
            yield False


def part1(lines):
    """
    >>> part1(["30373", "25512", "65332", "33549", "35390"])
    21
    """
    lines = [line.rstrip() for line in lines]
    visibilities = [list(_scan_visibility(line)) for line in lines]
    for line, row in zip(lines, visibilities):
        for col_index, value in enumerate(_scan_visibility(line[::-1])):
            row[-1 - col_index] |= value
    for col_index, line in enumerate(zip(*lines)):
        line = "".join(line)
        for row_index, value in enumerate(_scan_visibility(line)):
            visibilities[row_index][col_index] |= value
        for row_index, value in enumerate(_scan_visibility(line[::-1])):
            visibilities[-1 - row_index][col_index] |= value
    return sum(sum(row) for row in visibilities)


def _scan_score(line):
    horizon = 0
    for index, height in enumerate(line):
        for horizon in range(index - 1, -1, -1):
            if line[horizon] >= height:
                break
        yield index - horizon


def part2(lines):
    """
    >>> part2(["30373", "25512", "65332", "33549", "35390"])
    8
    """
    lines = [line.rstrip() for line in lines]
    scores = [list(_scan_score(line)) for line in lines]
    for line, row in zip(lines, scores):
        for col_index, value in enumerate(_scan_score(line[::-1])):
            row[-1 - col_index] *= value
    for col_index, line in enumerate(zip(*lines)):
        line = "".join(line)
        for row_index, value in enumerate(_scan_score(line)):
            scores[row_index][col_index] *= value
        for row_index, value in enumerate(_scan_score(line[::-1])):
            scores[-1 - row_index][col_index] *= value
    return max(max(row) for row in scores)


parts = (part1, part2)
