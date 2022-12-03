package com.github.ephemient.aoc2022

class Day3(private val lines: List<String>) {
    fun part1(): Int = lines.sumOf {
        (it.items(end = it.length / 2) intersect it.items(start = it.length / 2)).sum()
    }

    fun part2(): Int = lines.chunked(3).sumOf { (a, b, c) ->
        (a.items() intersect b.items() intersect c.items()).single()
    }
}

private fun String.items(start: Int = 0, end: Int = length): Set<Int> = buildSet {
    for (i in start until end) {
        when (val c = get(i)) {
            in 'a'..'z' -> add(c - 'a' + 1)
            in 'A'..'Z' -> add(c - 'A' + 27)
        }
    }
}
