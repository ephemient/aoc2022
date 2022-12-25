package com.github.ephemient.aoc2022

@Day
class Day25(private val lines: List<String>) {
    @Day.Part
    fun part1(): String = buildString {
        var n = lines.sumOf { line ->
            line.fold(0L) { k, c ->
                5 * k + when (c) {
                    '=' -> -2
                    '-' -> -1
                    else -> c.digitToInt()
                }
            }
        }
        while (n != 0L) {
            append("012=-"[n.mod(5)])
            n = (n + 2).floorDiv(5)
        }
        reverse()
    }
}
