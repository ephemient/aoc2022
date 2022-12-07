package com.github.ephemient.aoc2022

@Day
class Day6(lines: List<String>) {
    private val input = lines.single()

    @Day.Part
    fun part1(): Int? = solve(4)

    @Day.Part
    fun part2(): Int? = solve(14)

    private fun solve(n: Int): Int? {
        val index = input.windowedSequence(n).indexOfFirst { it.toSet().size == n }
        return if (index < 0) null else index + n
    }
}
