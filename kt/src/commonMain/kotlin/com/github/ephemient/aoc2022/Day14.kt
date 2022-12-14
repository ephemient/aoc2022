package com.github.ephemient.aoc2022

@Day
class Day14(lines: List<String>) {
    private val blocks: Bitmap
    private val maxY: Int

    init {
        var maxX = 0
        var maxY = 0
        for (line in lines) {
            for (point in line.splitToSequence(" -> ")) {
                val (x, y) = point.split(',', limit = 2)
                maxX = maxOf(maxX, x.toInt())
                maxY = maxOf(maxY, y.toInt())
            }
        }
        this.maxY = maxY
        val width = maxOf(maxX + 1, 500 + maxY + 2)
        blocks = Bitmap(width, maxY + 2)
        for (line in lines) {
            line.splitToSequence(" -> ").map {
                val (x, y) = it.split(",", limit = 2)
                x.toInt() to y.toInt()
            }.zipWithNext { (x1, y1), (x2, y2) ->
                for (x in minOf(x1, x2)..maxOf(x1, x2)) {
                    for (y in minOf(y1, y2)..maxOf(y1, y2)) {
                        blocks[x, y] = true
                    }
                }
            }.count()
        }
    }

    @Day.Part
    fun part1(): Int {
        val blocks = Bitmap(blocks)
        return generateSequence { fall(blocks, maxY) }
            .takeWhile { it.second < maxY }
            .onEach { blocks[it.first, it.second] = true }
            .count()
    }

    @Day.Part
    fun part2(): Int {
        val blocks = Bitmap(blocks)
        return generateSequence { fall(blocks, maxY + 1) }
            .onEach { blocks[it.first, it.second] = true }
            .indexOf(500 to 0) + 1
    }

    private class Bitmap private constructor(private val data: BooleanArray, private val width: Int) {
        constructor(width: Int, height: Int) : this(BooleanArray(width * height), width)
        constructor(other: Bitmap) : this(other.data.copyOf(), other.width)

        operator fun get(x: Int, y: Int): Boolean {
            require(x in 0 until width)
            return data[x + y * width]
        }

        operator fun set(x: Int, y: Int, value: Boolean) {
            require(x in 0 until width)
            data[x + y * width] = value
        }
    }

    companion object {
        private fun fall(blocks: Bitmap, maxY: Int): IntPair {
            var x = 500
            for (y in 1..maxY) {
                when {
                    !blocks[x, y] -> {}
                    !blocks[x - 1, y] -> x--
                    !blocks[x + 1, y] -> x++
                    else -> return x to y - 1
                }
            }
            return x to maxY
        }
    }
}
