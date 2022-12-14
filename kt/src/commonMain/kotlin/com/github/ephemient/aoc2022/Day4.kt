package com.github.ephemient.aoc2022

@Day
class Day4(lines: List<String>) {
    @Suppress("DestructuringDeclarationWithTooManyEntries")
    private val input = lines.map {
        val (a, b, c, d) = it.split(',', '-', limit = 4)
        a.toInt()..b.toInt() to c.toInt()..d.toInt()
    }

    @Day.Part
    fun part1(): Int = input.count { (x, y) ->
        x.first >= y.first && x.last <= y.last || x.first <= y.first && x.last >= y.last
    }

    @Day.Part
    fun part2(): Int = input.count { (x, y) ->
        x.first <= y.last && x.last >= y.first
    }
}
