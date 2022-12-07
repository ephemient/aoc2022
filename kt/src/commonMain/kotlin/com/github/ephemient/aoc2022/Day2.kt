package com.github.ephemient.aoc2022

@Day
class Day2(lines: List<String>) {
    private val inputs = lines.map { IntPair(it[0] - 'A' + 1, it[2] - 'X' + 1) }

    @Day.Part
    fun part1(): Int = inputs.sumOf { (first, second) -> score(first, second) }

    @Day.Part
    fun part2(): Int = inputs.sumOf { (first, second) -> score(first, 1 + (first + second).mod(3)) }
}

private fun score(first: Int, second: Int): Int = (1 + second - first).mod(3) * 3 + second
