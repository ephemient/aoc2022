package com.github.ephemient.aoc2022

class Day6(lines: List<String>) {
    private val input = lines.single()

    fun part1(): Int? = solve(4)

    fun part2(): Int? = solve(14)

    private fun solve(n: Int): Int? {
        val index = input.windowedSequence(n).indexOfFirst { it.toSet().size == n }
        return if (index < 0) null else index + n
    }
}
