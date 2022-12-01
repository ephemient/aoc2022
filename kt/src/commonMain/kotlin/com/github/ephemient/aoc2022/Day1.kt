package com.github.ephemient.aoc2022

class Day1(lines: List<String>) {
    private val sums = buildList {
        var sum = 0
        for (line in lines) {
            val value = line.toIntOrNull()
            if (value != null) {
                sum += value
            } else {
                add(sum)
                sum = 0
            }
        }
        add(sum)
    }

    fun part1(): Int = sums.max()

    fun part2(): Int = sums.sorted().takeLast(3).sum()
}
