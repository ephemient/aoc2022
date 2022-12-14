package com.github.ephemient.aoc2022

@Day
class Day14(lines: List<String>) {
    private val result = run {
        var maxX = 0
        var maxY = 0
        val points = lines.map { line ->
            line.split(" -> ").map {
                val (x, y) = it.split(',', limit = 2)
                x.toInt() to y.toInt()
            }.onEach { (x, y) ->
                check(x >= 0 && y >= 0)
                if (x > maxX) maxX = x
                if (y > maxY) maxY = y
            }
        }
        val width = maxOf(maxX, 500 + maxY + 1) + 1
        val blocks = BooleanArray(width * (maxY + 2))
        for (segments in points) {
            segments.reduce { point1, point2 ->
                for (x in minOf(point1.first, point2.first)..maxOf(point1.first, point2.first)) {
                    for (y in minOf(point1.second, point2.second)..maxOf(point1.second, point2.second)) {
                        blocks[x + y * width] = true
                    }
                }
                point2
            }
        }
        var count = 0
        var countAtMaxY = -1
        DeepRecursiveFunction<IntPair, Unit> { (x, y) ->
            check(x in 0 until width)
            if (blocks[x + y * width]) return@DeepRecursiveFunction
            if (y == maxY && countAtMaxY < 0) countAtMaxY = count
            if (y <= maxY) {
                callRecursive(x to y + 1)
                callRecursive(x - 1 to y + 1)
                callRecursive(x + 1 to y + 1)
            }
            blocks[x + y * width] = true
            count++
        }(500 to 0)
        check(countAtMaxY >= 0)
        countAtMaxY to count
    }

    @Day.Part
    fun part1(): Int = result.first

    @Day.Part
    fun part2(): Int = result.second
}
