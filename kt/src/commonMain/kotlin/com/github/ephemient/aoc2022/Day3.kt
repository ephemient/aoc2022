package com.github.ephemient.aoc2022

class Day3(private val lines: List<String>) {
    fun part1(): Int = lines.sumOf {
        (it.items(end = it.length / 2) and it.items(start = it.length / 2)).bitSum()
    }

    fun part2(): Int = lines.chunked(3).sumOf { (a, b, c) ->
        (a.items() and b.items() and c.items()).singleBit()
    }
}

private fun String.items(start: Int = 0, end: Int = length): Long {
    var set = 0L
    for (i in start until end) {
        when (val c = get(i)) {
            in 'a'..'z' -> set = set or 1L.shl(c - 'a' + 1)
            in 'A'..'Z' -> set = set or 1L.shl(c - 'A' + 27)
        }
    }
    return set
}

private fun Long.bitSum(): Int {
    var sum = 0
    for (i in 0 until 64) {
        if (this and 1L.shl(i) != 0L) sum += i
    }
    return sum
}

private fun Long.singleBit(): Int {
    require(this != 0L && this and this - 1 == 0L)
    return countTrailingZeroBits()
}
