package com.github.ephemient.aoc2022

@Day
class Day14(lines: List<String>) {
    private val blocks = buildSet {
        for (line in lines) {
            line.splitToSequence(" -> ").map {
                val (x, y) = it.split(",", limit = 2)
                x.toInt() to y.toInt()
            }.zipWithNext { (x1, y1), (x2, y2) ->
                for (x in minOf(x1, x2)..maxOf(x1, x2)) {
                    for (y in minOf(y1, y2)..maxOf(y1, y2)) {
                        add(x to y)
                    }
                }
            }.count()
        }
    }
    private val maxY = blocks.maxOf { it.second }

    @Day.Part
    fun part1(): Int {
        val blocks = blocks.toMutableSet()
        return generateSequence { fall(blocks, maxY) }.takeWhile { it.second < maxY }.onEach { blocks.add(it) }.count()
    }

    @Day.Part
    fun part2(): Int {
        val blocks = blocks.toMutableSet()
        return generateSequence { fall(blocks, maxY + 1) }.onEach { blocks.add(it) }.indexOf(500 to 0) + 1
    }
}

private fun fall(blocks: Set<IntPair>, maxY: Int): IntPair {
    var x = 500
    for (y in 1..maxY) {
        when {
            x to y !in blocks -> {}
            x - 1 to y !in blocks -> x--
            x + 1 to y !in blocks -> x++
            else -> return x to y - 1
        }
    }
    return x to maxY
}
